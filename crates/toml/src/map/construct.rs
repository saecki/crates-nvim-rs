use bumpalo::Bump;
use common::Span;
use indexmap::map::Entry::{Occupied, Vacant};
use indexmap::map::VacantEntry;

use crate::map::parent::{
    Complete, Incomplete, ParentInlineArray, ParentInlineArrayEntry, ParentTableEntry,
    ParentToplevelArray, ParentToplevelArrayEntry, ParentToplevelArrayExtensionEntry, ReprIdx,
    cyclic, cyclic_slice,
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
use crate::{Ast, MapTable};

#[derive(Default)]
pub struct Mapper<'a> {
    pub errors: Vec<MapError<'a, Incomplete>>,
}

impl<'a> Mapper<'a> {
    pub fn error(&mut self, error: MapError<'a, Incomplete>) {
        self.errors.push(error);
    }
}

pub fn map<'a>(
    bump: &'a Bump,
    ast: &'_ Ast<'a>,
) -> (Vec<MapError<'a, Complete>>, &'a MapTable<'a>) {
    let mut ctx = Mapper::default();
    let root = cyclic::<MapTable<'a, Incomplete>>(bump, |ptr| {
        let parent = ParentTable::new(ptr, ReprIdx(0));
        let mut root = MapTable::new(MapTableRepr::Root(ast.span));
        for t in ast.toplevel.iter() {
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

            insert_node_at_path(&mut ctx, bump, parent, &mut root.inner, key, value);
        }
        root
    });

    // SAFETY: The map has been fully constructed, and all cyclic references
    // should be valid. The generic tag doesn't have any effect on memory layout.
    unsafe {
        (
            std::mem::transmute::<Vec<MapError<Incomplete>>, Vec<MapError<Complete>>>(ctx.errors),
            std::mem::transmute::<&mut MapTable<Incomplete>, &mut MapTable<Complete>>(root),
        )
    }
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
    parent_entry: ParentEntry<'a, Incomplete>,
    value: InsertValue<'a>,
) -> MapNode<'a, Incomplete> {
    match value {
        InsertValue::Table(table) => {
            let map = cyclic(bump, |ptr| {
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
            let array = cyclic(bump, |ptr| {
                let parent = ParentToplevelArray::new(ptr);

                let array_entry = cyclic(bump, |ptr| {
                    let parent_array_entry = ParentToplevelArrayEntry::new(ptr).wrap();

                    let map = cyclic(bump, |ptr| {
                        let parent = ParentTable::new(ptr, ReprIdx(0));
                        let mut map =
                            MapTable::new(MapTableRepr::ArrayEntry(array_repr, parent_array_entry));
                        insert_top_level_assignments(
                            ctx,
                            bump,
                            parent,
                            &mut map.inner,
                            &array_repr.assignments,
                        );
                        map
                    });

                    MapArrayToplevelEntry::new(map, array_repr, parent_entry, parent, 0)
                });

                MapArrayToplevel::new(array_entry)
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
    parent_entry: ParentEntry<'a, Incomplete>,
    value: &'a Value<'a>,
) -> MapNode<'a, Incomplete> {
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
            let array = cyclic(bump, |ptr| {
                let parent = ParentInlineArray::new(ptr);
                let entries = cyclic_slice(bump, &inline_array.values, |idx, ptr, val| {
                    let parent_entry = ParentInlineArrayEntry::new(ptr).wrap();
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
    mut parent: ParentTable<'a, Incomplete>,
    map: &mut MapInner<'a, Incomplete>,
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

        let key_repr = MapTableKeyRepr::Dotted(i, idents);
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
    mut parent: ParentTable<'a, Incomplete>,
    mut vacant: VacantEntry<'_, &'a str, &'a mut MapTableEntry<'a, Incomplete>>,
    idents: &'a [DottedIdent<'a>],
    i: u32,
    value: InsertValue<'a>,
) {
    for (pair, i) in idents[i as usize..].windows(2).zip(i..) {
        let table_entry = cyclic(bump, |ptr| {
            let parent_entry = ParentTableEntry::new(ptr, ReprIdx(0)).wrap();
            let key_repr = MapTableKeyRepr::Dotted(i, idents);
            let repr = MapTableEntryRepr::new(parent, key_repr, value.repr_kind());

            let map = cyclic(bump, |ptr| {
                parent = ParentTable::new(ptr, ReprIdx(0));
                MapTable::new(value.repr_kind().table_repr(parent_entry))
            });

            MapTableEntry::new(MapNode::Table(map), repr)
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

    let table_entry = cyclic(bump, |ptr| {
        let parent_entry = ParentTableEntry::new(ptr, ReprIdx(0)).wrap();
        let key_repr = MapTableKeyRepr::Dotted((idents.len() - 1) as u32, idents);
        let repr = MapTableEntryRepr::new(parent, key_repr, value.repr_kind());
        let node = map_insert_value(ctx, bump, parent_entry, value);
        MapTableEntry::new(node, repr)
    });
    vacant.insert(table_entry);
}

fn insert_node<'a>(
    ctx: &mut Mapper<'a>,
    bump: &'a Bump,
    map: &mut MapInner<'a, Incomplete>,
    key: &'a Ident<'a>,
    value: InsertValue<'a>,
    repr: MapTableEntryRepr<'a, Incomplete>,
) -> Result<(), MapError<'a, Incomplete>> {
    let existing_entry = match map.entry(key.text) {
        Occupied(occupied) => occupied.into_mut(),
        Vacant(vacant) => {
            let table_entry = cyclic(bump, |ptr| {
                let parent_entry = ParentTableEntry::new(ptr, ReprIdx(0)).wrap();
                let node = map_insert_value(ctx, bump, parent_entry, value);
                MapTableEntry::new(node, repr)
            });
            vacant.insert(table_entry);
            return Ok(());
        }
    };

    // TODO: Should happen only if the entry is inserted?
    let parent_entry = ParentTableEntry::insert_repr(bump, existing_entry, repr).wrap();

    match value {
        InsertValue::Table(table) => {
            insert_table(ctx, bump, existing_entry, parent_entry, table, repr)
        }
        InsertValue::ArrayEntry(array_entry) => {
            insert_array_entry(ctx, bump, existing_entry, parent_entry, array_entry, repr)
        }
        InsertValue::ToplevelAssignment(_) | InsertValue::InlineTableAssignment(_) => {
            Err(map_error(
                MapErrorKind::DuplicateKey,
                existing_entry.reprs.first(),
                None,
                &repr,
            ))
        }
    }
}

fn insert_table<'a>(
    ctx: &mut Mapper<'a>,
    bump: &'a Bump,
    existing_entry: &mut &'a mut MapTableEntry<'a, Incomplete>,
    parent_entry: ParentEntry<'a, Incomplete>,
    table: &'a Table<'a>,
    repr: MapTableEntryRepr<'a, Incomplete>,
) -> Result<(), MapError<'a, Incomplete>> {
    let existing_table = match &mut existing_entry.node {
        MapNode::Table(table) => table,
        MapNode::Array(_) | MapNode::Scalar(_) => {
            return Err(map_error(
                MapErrorKind::DuplicateKey,
                existing_entry.reprs.first(),
                None,
                &repr,
            ));
        }
    };
    // The last repr is the one that is currently being inserted.
    for existing_repr in existing_entry.reprs[..existing_entry.reprs.len() - 1].iter() {
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
                return Err(map_error(
                    MapErrorKind::DuplicateKey,
                    existing_repr,
                    None,
                    &repr,
                ));
            }
        }
    }

    // Extend existing table with items from super table.
    // ```toml
    // [a.b.c] # this would be the existing table
    //
    // [a.b] # this would be the super table
    // ```
    let parent = ParentTable::insert_repr(bump, existing_table, repr.kind.table_repr(parent_entry));
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
    existing_entry: &mut &'a mut MapTableEntry<'a, Incomplete>,
    parent_entry: ParentEntry<'a, Incomplete>,
    array_repr: &'a ArrayEntry<'a>,
    repr: MapTableEntryRepr<'a, Incomplete>,
) -> Result<(), MapError<'a, Incomplete>> {
    let array = match &mut existing_entry.node {
        MapNode::Array(MapArray::Toplevel(a)) => a,
        MapNode::Array(MapArray::Inline(_)) => {
            let orig = existing_entry.reprs.first();
            return Err(map_error(
                MapErrorKind::CannotExtendInlineArray,
                orig,
                Some(orig.repr_span()),
                &repr,
            ));
        }
        MapNode::Table(_) | MapNode::Scalar(_) => {
            return Err(map_error(
                MapErrorKind::DuplicateKey,
                existing_entry.reprs.first(),
                None,
                &repr,
            ));
        }
    };

    let parent = ParentToplevelArray::new_from(bump, array);

    let array_entry = cyclic(bump, |ptr| {
        let parent_array_entry = ParentToplevelArrayEntry::new(ptr).wrap();

        let map = cyclic(bump, |ptr| {
            let parent = ParentTable::new(ptr, ReprIdx(0));
            let mut map = MapTable::new(repr.kind.table_repr(parent_array_entry));
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
        MapArrayToplevelEntry::new(map, array_repr, parent_entry, parent, idx)
    });

    array.push(array_entry);

    Ok(())
}

fn insert_top_level_assignments<'a>(
    ctx: &mut Mapper<'a>,
    bump: &'a Bump,
    parent: ParentTable<'a, Incomplete>,
    map: &mut MapInner<'a, Incomplete>,
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

fn get_table_to_extend<'a, 'b>(
    bump: &'a Bump,
    entry: &'b mut &'a mut MapTableEntry<'a, Incomplete>,
    repr: MapTableEntryRepr<'a, Incomplete>,
) -> Result<
    (
        ParentTable<'a, Incomplete>,
        &'b mut MapInner<'a, Incomplete>,
    ),
    MapError<'a, Incomplete>,
> {
    let parent_table_entry = ParentTableEntry::insert_repr(bump, entry, repr);

    let (parent, map) = match &mut entry.node {
        MapNode::Table(map) => {
            let table_repr = repr.kind.table_repr(parent_table_entry.wrap());
            let parent = ParentTable::insert_repr(bump, map, table_repr);
            (parent, map)
        }
        MapNode::Array(MapArray::Toplevel(array)) => {
            if repr.kind.is_assignment() {
                return Err(map_error(
                    MapErrorKind::CannotExtendArrayWithDottedKey,
                    entry.reprs.first(),
                    Some(array.inner.first().definition.header.span()),
                    &repr,
                ));
            }

            // From the toml spec (https://toml.io/en/v1.0.0#array-of-tables):
            // Any reference to an array of tables points to the most recently
            // defined table element of the array. This allows you to define
            // sub-tables, and even sub-arrays of tables, inside the most recent
            // table.

            let array_entry = array.inner.last_mut();

            let parent_extension_entry =
                ParentToplevelArrayExtensionEntry::insert(array_entry, parent_table_entry).wrap();

            let map = &mut array_entry.node;
            let parent =
                ParentTable::insert_repr(bump, map, repr.kind.table_repr(parent_extension_entry));
            (parent, map)
        }
        MapNode::Array(MapArray::Inline(_)) => {
            let orig = entry.reprs.first();
            return Err(map_error(
                MapErrorKind::CannotExtendInlineArrayAsTable,
                orig,
                Some(orig.repr_span()),
                &repr,
            ));
        }
        MapNode::Scalar(_) => {
            return Err(map_error(
                MapErrorKind::DuplicateKey,
                entry.reprs.first(),
                None,
                &repr,
            ));
        }
    };

    for existing in entry.reprs.iter() {
        match &existing.kind {
            MapTableEntryReprKind::Table(table) => {
                if repr.kind.is_assignment() {
                    return Err(map_error(
                        MapErrorKind::CannotExtendTableWithDottedKey,
                        entry.reprs.first(),
                        Some(table.header.span()),
                        &repr,
                    ));
                }
            }
            MapTableEntryReprKind::ArrayEntry(_) => (),
            MapTableEntryReprKind::ToplevelAssignment(_)
            | MapTableEntryReprKind::InlineTableAssignment(_) => {
                if existing.key.is_last_ident() {
                    // `map` is an inline table
                    let orig = entry.reprs.first();
                    return Err(map_error(
                        MapErrorKind::CannotExtendInlineTable,
                        orig,
                        Some(orig.repr_span()),
                        &repr,
                    ));
                }
            }
        }
    }

    Ok((parent, &mut map.inner))
}

pub struct MapError<'a, S = Complete> {
    pub kind: MapErrorKind,
    pub orig_parent: ParentTable<'a, S>,
    pub orig_span: Span,
    pub new_parent: ParentTable<'a, S>,
    pub new_ident: &'a Ident<'a>,
}

pub enum MapErrorKind {
    DuplicateKey,
    CannotExtendTableWithDottedKey,
    CannotExtendInlineTable,
    CannotExtendArrayWithDottedKey,
    CannotExtendInlineArray,
    CannotExtendInlineArrayAsTable,
}

fn map_error<'a>(
    kind: MapErrorKind,
    orig: &MapTableEntryRepr<'a, Incomplete>,
    orig_span: Option<Span>,
    new: &MapTableEntryRepr<'a, Incomplete>,
) -> MapError<'a, Incomplete> {
    MapError {
        kind,
        orig_parent: orig.parent,
        orig_span: orig_span.unwrap_or(orig.key.repr_ident().lit_span()),
        new_parent: new.parent,
        new_ident: new.key.repr_ident(),
    }
}
