use crate::MapTable;
use crate::map::cyclic::{Complete, Cyclic, CyclicCell, Incomplete};
use crate::map::{
    MapArrayInline, MapArrayInlineEntry, MapArrayToplevel, MapArrayToplevelEntry, MapTableEntry,
    MapTableEntryRepr, MapTableRepr,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ParentTable<'a, S = Complete> {
    ptr: Cyclic<'a, MapTable<'a, S>, S>,
    idx: ReprIdx,
}

impl<'a, S> ParentTable<'a, S> {
    pub(super) fn new(ptr: Cyclic<'a, MapTable<'a, S>, S>, idx: ReprIdx) -> Self {
        Self { ptr, idx }
    }
}

impl<'a> ParentTable<'a, Incomplete> {
    pub(super) fn insert_repr(
        map: &mut CyclicCell<'a, MapTable<'a, Incomplete>, Incomplete>,
        repr: MapTableRepr<'a, Incomplete>,
    ) -> Self {
        let inner = map.get_mut();
        let idx = ReprIdx(inner.reprs.len() as u32);
        inner.reprs.push(repr);

        Self::new(map.cyclic_ref(), idx)
    }
}

impl<'a> ParentTable<'a, Complete> {
    pub fn get(self) -> &'a MapTable<'a> {
        self.ptr.get()
    }

    pub fn repr(self) -> &'a MapTableRepr<'a> {
        &self.ptr.get().reprs[self.idx.idx()]
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ParentToplevelArray<'a, S = Complete> {
    ptr: Cyclic<'a, MapArrayToplevel<'a, S>, S>,
}

impl<'a, S> ParentToplevelArray<'a, S> {
    pub(super) fn new(ptr: Cyclic<'a, MapArrayToplevel<'a, S>, S>) -> Self {
        Self { ptr }
    }
}

impl<'a> ParentToplevelArray<'a, Incomplete> {
    pub(super) fn new_from(
        array: &mut CyclicCell<'a, MapArrayToplevel<'a, Incomplete>, Incomplete>,
    ) -> Self {
        Self::new(array.cyclic_ref())
    }
}

impl<'a> ParentToplevelArray<'a, Complete> {
    pub fn get(self) -> &'a MapArrayToplevel<'a> {
        self.ptr.get()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ParentInlineArray<'a, S = Complete> {
    ptr: Cyclic<'a, MapArrayInline<'a, S>, S>,
}

impl<'a, S> ParentInlineArray<'a, S> {
    pub(super) fn new(ptr: Cyclic<'a, MapArrayInline<'a, S>, S>) -> Self {
        Self { ptr }
    }
}

impl<'a> ParentInlineArray<'a, Complete> {
    pub fn get(self) -> &'a MapArrayInline<'a> {
        self.ptr.get()
    }
}

impl<'a> std::ops::Deref for ParentInlineArray<'a, Complete> {
    type Target = MapArrayInline<'a>;

    fn deref(&self) -> &'a Self::Target {
        self.get()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ParentEntry<'a, S = Complete> {
    Table(ParentTableEntry<'a, S>),
    ToplevelArray(ParentToplevelArrayEntry<'a, S>),
    ToplevelArrayExtension(ParentToplevelArrayExtensionEntry<'a, S>),
    InlineArray(ParentInlineArrayEntry<'a, S>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ParentTableEntry<'a, S = Complete> {
    ptr: Cyclic<'a, MapTableEntry<'a, S>, S>,
    idx: ReprIdx,
}

impl<'a, S> ParentTableEntry<'a, S> {
    pub(super) fn new(ptr: Cyclic<'a, MapTableEntry<'a, S>, S>, idx: ReprIdx) -> Self {
        Self { ptr, idx }
    }

    pub fn wrap(self) -> ParentEntry<'a, S> {
        ParentEntry::Table(self)
    }
}

impl<'a> ParentTableEntry<'a, Incomplete> {
    pub(super) fn insert_repr(
        entry: &mut CyclicCell<'a, MapTableEntry<'a, Incomplete>, Incomplete>,
        repr: MapTableEntryRepr<'a, Incomplete>,
    ) -> Self {
        let inner = entry.get_mut();
        let idx = ReprIdx(inner.reprs.len() as u32);
        inner.reprs.push(repr);

        Self::new(entry.cyclic_ref(), idx)
    }
}

impl<'a> ParentTableEntry<'a, Complete> {
    pub fn get(self) -> &'a MapTableEntry<'a> {
        self.ptr.get()
    }

    pub fn repr(self) -> &'a MapTableEntryRepr<'a> {
        &self.ptr.get().reprs[self.idx.idx()]
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ParentToplevelArrayEntry<'a, S = Complete> {
    ptr: Cyclic<'a, MapArrayToplevelEntry<'a, S>, S>,
}

impl<'a, S> ParentToplevelArrayEntry<'a, S> {
    pub(super) fn new(ptr: Cyclic<'a, MapArrayToplevelEntry<'a, S>, S>) -> Self {
        Self { ptr }
    }

    pub fn wrap(self) -> ParentEntry<'a, S> {
        ParentEntry::ToplevelArray(self)
    }
}

impl<'a> ParentToplevelArrayEntry<'a, Complete> {
    pub fn get(self) -> &'a MapArrayToplevelEntry<'a> {
        self.ptr.get()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ParentToplevelArrayExtensionEntry<'a, S> {
    ptr: Cyclic<'a, MapArrayToplevelEntry<'a, S>, S>,
    idx: ReprIdx,
}

impl<'a, S> ParentToplevelArrayExtensionEntry<'a, S> {
    pub(super) fn new(ptr: Cyclic<'a, MapArrayToplevelEntry<'a, S>, S>, idx: ReprIdx) -> Self {
        Self { ptr, idx }
    }

    pub fn wrap(self) -> ParentEntry<'a, S> {
        ParentEntry::ToplevelArrayExtension(self)
    }
}

impl<'a> ParentToplevelArrayExtensionEntry<'a, Incomplete> {
    pub(super) fn insert(
        array_entry: &mut CyclicCell<'a, MapArrayToplevelEntry<'a, Incomplete>, Incomplete>,
        parent_entry: ParentTableEntry<'a, Incomplete>,
    ) -> Self {
        let array_entry_inner = array_entry.get_mut();
        let idx = ReprIdx(array_entry_inner.extensions.len() as u32);
        array_entry_inner.extensions.push(parent_entry);

        let ptr = array_entry.cyclic_ref();
        Self::new(ptr, idx)
    }
}

impl<'a> ParentToplevelArrayExtensionEntry<'a, Complete> {
    pub fn get(self) -> &'a MapArrayToplevelEntry<'a> {
        self.ptr.get()
    }

    pub fn parent_table_entry(self) -> ParentTableEntry<'a> {
        self.ptr.get().extensions[self.idx.idx()]
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ParentInlineArrayEntry<'a, S = Complete> {
    ptr: Cyclic<'a, MapArrayInlineEntry<'a, S>, S>,
}

impl<'a, S> ParentInlineArrayEntry<'a, S> {
    pub(super) fn new(ptr: Cyclic<'a, MapArrayInlineEntry<'a, S>, S>) -> Self {
        Self { ptr }
    }

    pub fn wrap(self) -> ParentEntry<'a, S> {
        ParentEntry::InlineArray(self)
    }
}

impl<'a> ParentInlineArrayEntry<'a, Complete> {
    pub fn get(self) -> &'a MapArrayInlineEntry<'a> {
        self.ptr.get()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ReprIdx(pub u32);

impl ReprIdx {
    pub fn idx(self) -> usize {
        self.0 as usize
    }
}
