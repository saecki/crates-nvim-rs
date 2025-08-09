use bumpalo::Bump;
use common::OneVec;
use indexmap::map::Entry::{Occupied, Vacant};
use indexmap::map::{OccupiedEntry, VacantEntry};

use crate::map::parent::{
    ParentInlineArray, ParentInlineArrayEntry, ParentTableEntry, ParentToplevelArray,
    ParentToplevelArrayEntry, ReprIdx, cyclic, cyclic_slice,
};
use crate::map::{
    MapArray, MapArrayInline, MapArrayInlineEntry, MapArrayToplevel, MapArrayToplevelEntry,
    MapInner, MapNode, MapTableEntry, MapTableEntryRepr, MapTableEntryReprKind, MapTableKeyRepr,
    MapTableRepr, ParentEntry, ParentTable, Scalar,
};
use crate::parse::{
    ArrayEntry, DottedIdent, Ident, InlineTableAssignment, Key, Table, Toplevel,
    ToplevelAssignment, Value,
};
use crate::{Ast, Error, MapTable};

#[derive(Default)]
pub struct Mapper<'a> {
    pub errors: Vec<MapError<'a>>,
}

impl<'a> Mapper<'a> {
    pub fn error(&mut self, error: MapError<'a>) {
        self.errors.push(error);
    }
}

pub fn map<'a>(ctx: &mut Mapper<'a>, bump: &'a Bump, ast: &'_ Ast<'a>) -> &'a MapTable<'a> {
    cyclic::<MapTable>(bump, |ptr| {
        let mut root = MapTable::new(MapTableRepr::Root(ast.span));
        for (t, i) in ast.toplevel.iter().zip(0..) {
            let parent = ParentTable::new(ptr, ReprIdx(i));
            let (key, value) = match t {
                Toplevel::Assignment(assignment) => {
                    let key = &assignment.assignment.key;
                    let value = InsertValue::ToplevelAssignment(assignment);
                    (key, value)
                }
                Toplevel::Table(table) => {
                    let Some(key) = &table.header.key else {
                        continue;
                    };
                    (key, InsertValue::Table(table))
                }
                Toplevel::Array(array_entry) => {
                    let Some(key) = &array_entry.header.key else {
                        continue;
                    };
                    (key, InsertValue::ArrayEntry(array_entry))
                }
            };

            insert_node_at_path(ctx, &bump, parent, &mut root.inner, key, value);
        }
        root
    })
}

/// Value to be lazily mapped and inserted
#[derive(Clone, Copy)]
enum InsertValue<'a> {
    Table(&'a Table<'a>),
    ArrayEntry(&'a ArrayEntry<'a>),
    ToplevelAssignment(&'a ToplevelAssignment<'a>),
    InlineTableAssignment(&'a InlineTableAssignment<'a>),
}

impl<'a> InsertValue<'a> {
    fn repr_kind(self) -> MapTableEntryReprKind<'a> {
        match self {
            Self::Table(table) => MapTableEntryReprKind::Table(table),
            Self::ArrayEntry(array_entry) => MapTableEntryReprKind::ArrayEntry(array_entry),
            Self::ToplevelAssignment(assignment) => {
                MapTableEntryReprKind::ToplevelAssignment(assignment)
            }
            Self::InlineTableAssignment(assignment) => {
                MapTableEntryReprKind::InlineTableAssignment(assignment)
            }
        }
    }
}

fn map_insert_value<'a>(
    ctx: &mut Mapper<'a>,
    bump: &'a Bump,
    parent_entry: ParentEntry<'a>,
    value: InsertValue<'a>,
) -> MapNode<'a> {
    match value {
        InsertValue::Table(table) => {
            let map = cyclic::<MapTable>(bump, |ptr| {
                let mut map = MapTable::new(MapTableRepr::Table(table, parent_entry));
                insert_top_level_assignments(
                    ctx,
                    bump,
                    ParentTable::new(ptr, ReprIdx(0)),
                    &mut map.inner,
                    &table.assignments,
                );
                map
            });
            MapNode::Table(map)
        }
        InsertValue::ArrayEntry(array_repr) => {
            let array = cyclic::<MapArrayToplevel>(bump, |ptr| {
                let parent = ParentToplevelArray::new(ptr);

                let array_entry = cyclic::<MapArrayToplevelEntry>(bump, |ptr| {
                    let parent_entry =
                        ParentEntry::ToplevelArray(ParentToplevelArrayEntry::new(ptr));

                    let map = cyclic::<MapTable>(bump, |ptr| {
                        let parent = ParentTable::new(ptr, ReprIdx(0));
                        let mut map =
                            MapTable::new(MapTableRepr::ArrayEntry(array_repr, parent_entry));
                        insert_top_level_assignments(
                            ctx,
                            bump,
                            parent,
                            &mut map.inner,
                            &array_repr.assignments,
                        );
                        map
                    });

                    MapArrayToplevelEntry::new(map, array_repr, parent, 0)
                });

                MapArrayToplevel::new(array_entry, parent_entry)
            });
            MapNode::Array(MapArray::Toplevel(array))
        }
        InsertValue::ToplevelAssignment(assignment) => {
            map_value(ctx, bump, parent_entry, &assignment.assignment.val)
        }
        InsertValue::InlineTableAssignment(assignment) => {
            map_value(ctx, bump, parent_entry, &assignment.assignment.val)
        }
    }
}

fn map_value<'a>(
    ctx: &mut Mapper<'a>,
    bump: &'a Bump,
    parent_entry: ParentEntry<'a>,
    value: &'a Value<'a>,
) -> MapNode<'a> {
    match value {
        Value::String(s) => MapNode::Scalar(Scalar::String(s)),
        Value::Int(i) => MapNode::Scalar(Scalar::Int(i)),
        Value::Float(f) => MapNode::Scalar(Scalar::Float(f)),
        Value::Bool(b) => MapNode::Scalar(Scalar::Bool(b)),
        Value::DateTime(d) => MapNode::Scalar(Scalar::DateTime(d)),
        Value::InlineTable(table) => {
            let map = cyclic(bump, |ptr| {
                let mut map = MapTable::new(MapTableRepr::InlineTable(table, parent_entry));
                for (assignment, i) in table.assignments.iter().zip(0..) {
                    insert_node_at_path(
                        ctx,
                        bump,
                        ParentTable::new(ptr, ReprIdx(i)),
                        &mut map.inner,
                        &assignment.assignment.key,
                        InsertValue::InlineTableAssignment(assignment),
                    );
                }
                map
            });
            MapNode::Table(map)
        }
        Value::InlineArray(inline_array) => {
            let array = cyclic::<MapArrayInline>(bump, |ptr| {
                let parent = ParentInlineArray::new(ptr);
                let entries = cyclic_slice(bump, &inline_array.values, |idx, ptr, val| {
                    let parent_entry = ParentEntry::InlineArray(ParentInlineArrayEntry::new(ptr));
                    let node = map_value(ctx, bump, parent_entry, &val.val);
                    MapArrayInlineEntry::new(node, val, parent, idx)
                });

                MapArrayInline::new(parent_entry, inline_array, entries)
            });
            MapNode::Array(MapArray::Inline(array))
        }
        Value::Invalid(s) => MapNode::Scalar(Scalar::Invalid(s)),
    }
}

#[allow(clippy::too_many_arguments)]
fn insert_node_at_path<'a>(
    ctx: &mut Mapper<'a>,
    bump: &'a Bump,
    mut parent: ParentTable<'a>,
    map: &mut MapInner<'a>,
    key: &'a Key<'a>,
    value: InsertValue<'a>,
) {
    let idents = match key {
        Key::One(ident) => {
            let key_repr = MapTableKeyRepr::One(ident);
            let repr = MapTableEntryRepr::new(parent, key_repr, value.repr_kind());
            let res = insert_node(ctx, bump, map, ident, value, repr);
            if let Err(e) = res {
                ctx.error(e);
            }
            return;
        }
        Key::Dotted(idents) => idents,
    };

    let [other @ .., last] = idents else {
        unreachable!()
    };
    let mut current = map;
    for (o, i) in other.iter().zip(0..) {
        let entry = match current.entry(o.ident.text) {
            Occupied(occupied) => occupied.into_mut(),
            Vacant(vacant) => {
                insert_at_vacant_path(ctx, bump, parent, vacant, idents, i, value);
                return;
            }
        };

        let key_repr = MapTableKeyRepr::Dotted(i as u32, idents);
        let repr = MapTableEntryRepr::new(parent, key_repr, value.repr_kind());
        match get_table_to_extend(bump, entry, repr) {
            Ok((next_parent, next)) => {
                parent = next_parent;
                current = next;
            }
            Err(e) => {
                ctx.error(e);
                return;
            }
        };
    }

    let key_repr = MapTableKeyRepr::Dotted((idents.len() - 1) as u32, idents);
    let repr = MapTableEntryRepr::new(parent, key_repr, value.repr_kind());
    let res = insert_node(ctx, bump, current, &last.ident, value, repr);
    if let Err(e) = res {
        ctx.error(e);
    }
}

fn insert_at_vacant_path<'a>(
    ctx: &mut Mapper<'a>,
    bump: &'a Bump,
    mut parent: ParentTable<'a>,
    mut vacant: VacantEntry<'_, &'a str, &'a mut MapTableEntry<'a>>,
    idents: &'a [DottedIdent<'a>],
    i: u32,
    value: InsertValue<'a>,
) {
    for (pair, i) in idents[i as usize..].windows(2).zip(i..) {
        let table_entry = cyclic::<MapTableEntry>(bump, |ptr| {
            let parent_entry = ParentEntry::Table(ParentTableEntry::new(ptr, ReprIdx(0)));
            let key_repr = MapTableKeyRepr::Dotted(i, idents);
            let repr = MapTableEntryRepr::new(parent, key_repr, value.repr_kind());

            let map = cyclic::<MapTable>(bump, |ptr| {
                parent = ParentTable::new(ptr, ReprIdx(0));
                MapTable::new(value.repr_kind().table_repr(parent_entry))
            });

            MapTableEntry::from_one(MapNode::Table(map), repr)
        });
        let entry = vacant.insert(table_entry);

        let MapNode::Table(next) = &mut entry.node else {
            unreachable!()
        };
        vacant = match next.inner.entry(pair[1].ident.text) {
            Occupied(_) => unreachable!(),
            Vacant(vacant) => vacant,
        };
    }

    let table_entry = cyclic::<MapTableEntry>(bump, |ptr| {
        let parent_entry = ParentEntry::Table(ParentTableEntry::new(ptr, ReprIdx(0)));
        let key_repr = MapTableKeyRepr::Dotted((idents.len() - 1) as u32, idents);
        let repr = MapTableEntryRepr::new(parent, key_repr, value.repr_kind());
        let node = map_insert_value(ctx, bump, parent_entry, value);
        MapTableEntry::from_one(node, repr)
    });
    vacant.insert(table_entry);
}

fn insert_node<'a>(
    ctx: &mut Mapper<'a>,
    bump: &'a Bump,
    map: &mut MapInner<'a>,
    key: &'a Ident<'a>,
    value: InsertValue<'a>,
    repr: MapTableEntryRepr<'a>,
) -> Result<(), MapError<'a>> {
    let mut existing_entry = match map.entry(key.text) {
        Occupied(occupied) => occupied.into_mut(),
        Vacant(vacant) => {
            let table_entry = cyclic::<MapTableEntry>(bump, |ptr| {
                let parent_entry = ParentEntry::Table(ParentTableEntry::new(ptr, ReprIdx(0)));
                let node = map_insert_value(ctx, bump, parent_entry, value);
                MapTableEntry::from_one(node, repr)
            });
            vacant.insert(table_entry);
            return Ok(());
        }
    };

    // TODO: Should happen only if the entry is inserted?
    let parent_entry = ParentTableEntry::insert_repr(bump, &mut existing_entry, repr);

    match value {
        InsertValue::Table(table) => {
            insert_table(ctx, bump, existing_entry, parent_entry, table, repr)
        }
        InsertValue::ArrayEntry(array_entry) => {
            insert_array_entry(ctx, bump, existing_entry, parent_entry, array_entry, repr)
        }
        InsertValue::ToplevelAssignment(value) => todo!("duplicate key error"),
        InsertValue::InlineTableAssignment(value) => todo!("duplicate key error"),
    }
}

fn insert_table<'a>(
    ctx: &mut Mapper<'a>,
    bump: &'a Bump,
    existing_entry: &mut &'a mut MapTableEntry<'a>,
    parent_entry: ParentEntry<'a>,
    table: &'a Table<'a>,
    repr: MapTableEntryRepr<'a>,
) -> Result<(), MapError<'a>> {
    let mut existing_table = match &mut existing_entry.node {
        MapNode::Table(table) => table,
        MapNode::Array(_) | MapNode::Scalar(_) => {
            return Err(duplicate_key_error(existing_entry.reprs.first(), &repr));
        }
    };
    for existing_repr in existing_entry.reprs.iter() {
        match existing_repr.kind {
            MapTableEntryReprKind::Table(_) | MapTableEntryReprKind::ArrayEntry(_)
                if !existing_repr.key.is_last_ident() =>
            {
                // allow super tables, that are declared out of order
            }
            MapTableEntryReprKind::Table(_)
            | MapTableEntryReprKind::ArrayEntry(_)
            | MapTableEntryReprKind::ToplevelAssignment(_)
            | MapTableEntryReprKind::InlineTableAssignment(_) => {
                return Err(duplicate_key_error(existing_repr, &repr));
            }
        }
    }

    // Extend existing table with items from super table.
    // ```toml
    // [a.b.c] # this would be the existing table
    //
    // [a.b] # this would be the super table
    // ```
    let parent = ParentTable::insert_repr(
        bump,
        &mut existing_table,
        repr.kind.table_repr(parent_entry),
    );
    insert_top_level_assignments(
        ctx,
        bump,
        parent,
        &mut existing_table.inner,
        &table.assignments,
    );

    Ok(())
}

fn insert_array_entry<'a>(
    ctx: &mut Mapper<'a>,
    bump: &'a Bump,
    existing_entry: &mut &'a mut MapTableEntry<'a>,
    parent_entry: ParentEntry<'a>,
    array_repr: &'a ArrayEntry<'a>,
    repr: MapTableEntryRepr<'a>,
) -> Result<(), MapError<'a>> {
    let array = match &mut existing_entry.node {
        MapNode::Array(MapArray::Toplevel(a)) => a,
        MapNode::Array(MapArray::Inline(_)) => {
            let orig = existing_entry.reprs.first();
            return Err(MapError::CannotExtendInlineArray {
                lines: context_lines(path, [orig.parent, repr.parent]),
                path: joined_path(path, repr.key.repr_ident()),
                orig: orig.kind.span(),
                new: repr.key.repr_ident().lit_span(),
            });
        }
        MapNode::Table(_) | MapNode::Scalar(_) => {
            return Err(duplicate_key_error(existing_entry.reprs.first(), &repr));
        }
    };

    let parent = ParentToplevelArray::new_from(bump, &mut array);

    let array_entry = cyclic::<MapArrayToplevelEntry>(bump, |ptr| {
        let parent_entry = ParentEntry::ToplevelArray(ParentToplevelArrayEntry::new(ptr));

        let map = cyclic::<MapTable>(bump, |ptr| {
            let parent = ParentTable::new(ptr, ReprIdx(0));
            let mut map = MapTable::new(repr.kind.table_repr(parent_entry));
            insert_top_level_assignments(
                ctx,
                bump,
                parent,
                &mut map.inner,
                &array_repr.assignments,
            );
            map
        });

        let idx = array.len() as u32;
        MapArrayToplevelEntry::new(map, array_repr, parent, idx)
    });

    array.push(array_entry);

    Ok(())
}

fn insert_top_level_assignments<'a>(
    ctx: &mut Mapper<'a>,
    bump: &'a Bump,
    parent: ParentTable<'a>,
    map: &mut MapInner<'a>,
    assignments: &'a [ToplevelAssignment<'a>],
) {
    for assignment in assignments.iter() {
        insert_node_at_path(
            ctx,
            bump,
            parent,
            map,
            &assignment.assignment.key,
            InsertValue::ToplevelAssignment(assignment),
        );
    }
}

fn get_table_to_extend<'a>(
    bump: &'a Bump,
    entry: &mut &'a mut MapTableEntry<'a>,
    repr: MapTableEntryRepr<'a>,
) -> Result<(ParentTable<'a>, &'a mut MapInner<'a>), MapError<'a>> {
    let (parent, map) = match &mut entry.node {
        MapNode::Table(map) => {
            let parent_entry = ParentTableEntry::insert_repr(bump, entry, repr);
            let parent = ParentTable::insert_repr(bump, map, repr.kind.table_repr(parent_entry));
            (parent, map)
        }
        MapNode::Array(MapArray::Toplevel(array)) => {
            if repr.kind.is_assignment() {
                let orig = entry.reprs.first();
                return Err(Error::CannotExtendArrayWithDottedKey {
                    lines: context_lines(prev, [orig.parent, repr.parent]),
                    orig: orig.kind.span(),
                    path: joined_path(prev, repr.key.repr_ident()),
                    new: repr.key.repr_ident().lit_span(),
                });
            }

            // From the toml spec (https://toml.io/en/v1.0.0#array-of-tables):
            // Any reference to an array of tables points to the most recently
            // defined table element of the array. This allows you to define
            // sub-tables, and even sub-arrays of tables, inside the most recent
            // table.

            let parent_entry = ParentTableEntry::insert_repr(bump, entry, repr);
            // TODO: include the array entry in the parent hierarchy, so it can
            // be included in the context lines and the path.
            let array_entry = array.inner.last_mut();

            array_entry.parent

            let map = &mut array_entry.node;
            let parent = ParentTable::insert_repr(bump, map, repr.kind.table_repr(parent_entry));
            (parent, map)

        }
        MapNode::Array(MapArray::Inline(_)) => {
            let orig = entry.reprs.first();
            return Err(Error::CannotExtendInlineArrayAsTable {
                lines: context_lines(prev, [orig.parent, repr.parent]),
                path: joined_path(prev, repr.key.repr_ident()),
                orig: orig.kind.span(),
                new: repr.key.repr_ident().lit_span(),
            });
        }
        MapNode::Scalar(_) => {
            return Err(duplicate_key_error(entry.reprs.first(), &repr));
        }
    };

    for existing in entry.reprs.iter() {
        match &existing.kind {
            MapTableEntryReprKind::Table(_) => {
                if repr.kind.is_assignment() {
                    let orig = entry.reprs.first();
                    let dupe = entry.reprs.last();
                    return Err(Error::CannotExtendTableWithDottedKey {
                        lines: context_lines(prev, [orig.parent, dupe.parent]),
                        path: next_path.fmt_path(),
                        orig: orig.kind.span(),
                        new: dupe.key.repr_ident().lit_span(),
                    });
                }
            }
            MapTableEntryReprKind::ArrayEntry(_) => (),
            MapTableEntryReprKind::ToplevelAssignment(_)
            | MapTableEntryReprKind::InlineTableAssignment(_) => {
                if existing.key.is_last_ident() {
                    // `next` is an inline table
                    let orig = entry.reprs.first();
                    let dupe = entry.reprs.last();
                    return Err(Error::CannotExtendInlineTable {
                        lines: context_lines(prev, [orig.parent, dupe.parent]),
                        path: next_path.fmt_path(),
                        orig: orig.kind.span(),
                        new: entry.reprs.last().key.repr_ident().lit_span(),
                    });
                }
            }
        }
    }

    Ok((parent, &mut map.inner))
}

pub enum MapError<'a> {
    DuplicateKey {
        original_parent: ParentTable<'a>,
        original_ident: &'a Ident<'a>,
        duplicate_parent: ParentTable<'a>,
        duplicate_ident: &'a Ident<'a>,
    },
}

fn duplicate_key_error<'a>(
    original: &MapTableEntryRepr<'a>,
    duplicate: &MapTableEntryRepr<'a>,
) -> MapError<'a> {
    MapError::DuplicateKey {
        original_parent: original.parent,
        original_ident: original.key.repr_ident(),
        duplicate_parent: duplicate.parent,
        duplicate_ident: duplicate.key.repr_ident(),
    }
}
