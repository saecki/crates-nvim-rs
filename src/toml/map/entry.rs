use crate::toml::{MapNode, MapTable, MapTableEntries, MapTableEntryRepr};

impl<'a> MapTable<'a> {
    pub(super) fn entry<'b>(&'b mut self, key: &'a str) -> MapEntry<'a, 'b> {
        match self.inner.entry(key) {
            std::collections::hash_map::Entry::Occupied(inner) => {
                MapEntry::Occupied(OccupiedEntry { inner })
            }
            std::collections::hash_map::Entry::Vacant(inner) => {
                MapEntry::Vacant(VacantEntry { inner })
            }
        }
    }
}

pub(super) enum MapEntry<'a, 'b>
where
    'a: 'b,
{
    Occupied(OccupiedEntry<'a, 'b>),
    Vacant(VacantEntry<'a, 'b>),
}

pub(super) struct OccupiedEntry<'a, 'b>
where
    'a: 'b,
{
    inner: std::collections::hash_map::OccupiedEntry<'b, &'a str, MapTableEntries<'a>>,
}

impl<'a, 'b> OccupiedEntry<'a, 'b> {
    pub(super) fn existing_key(&'a self) -> &'a str {
        self.inner.key()
    }

    pub(super) fn get_mut(&mut self) -> &'b mut MapTableEntries<'a> {
        // SAFETY: `inner.get_mut()` returns a mutable reference (of lifetime 'b) to an entry inside
        // the HashMap, the function signature just fails to reflect that.
        unsafe { std::mem::transmute(self.inner.get_mut()) }
    }
}

pub(super) struct VacantEntry<'a, 'b>
where
    'a: 'b,
{
    inner: std::collections::hash_map::VacantEntry<'b, &'a str, MapTableEntries<'a>>,
}

impl<'a, 'b> VacantEntry<'a, 'b> {
    pub fn insert(
        self,
        node: MapNode<'a>,
        repr: MapTableEntryRepr<'a>,
    ) -> &'b mut MapTableEntries<'a> {
        self.inner.insert(MapTableEntries::new(node, repr))
    }
}
