use std::{collections::VecDeque, iter::FilterMap, ops::Deref, mem};

type AllocatorGeneration = u64;

#[derive(Debug, Copy, Clone, PartialEq, Default, std::cmp::Eq, Hash)]
pub struct Key {
    pub index: usize,
    pub generation: AllocatorGeneration,
}
#[derive(Debug, Clone)]
pub enum AllocatorSlot<T> {
    Free(AllocatorGeneration),
    Occupied(AllocatorGeneration, T),
}

#[derive(Debug, Clone)]
pub struct Allocator<T> {
    elements: Vec<AllocatorSlot<T>>, // the stored data managed by the allocator
    free_slots: VecDeque<usize>,     // list of the open slots we can insert into
}

impl<T> Deref for Allocator<T> {
    type Target = Vec<AllocatorSlot<T>>;

    fn deref(&self) -> &Self::Target {
        &self.elements
    }
}

impl<T> Allocator<T> {
    pub fn new() -> Self {
        Allocator {
            elements: Vec::new(),
            free_slots: VecDeque::new(),
        }
    }

    pub fn key_is_valid(&self, key: &Key) -> bool {
        // checks if a key is
        if let Some(value) = self.elements.get(key.index) {
            match value {
                AllocatorSlot::Occupied(generation, _) => *generation == key.generation,
                AllocatorSlot::Free(_) => false,
            }
        } else {
            false
        }
    }
    pub fn insert(&mut self, value: T) -> Key {
        if let Some(next_free_index) = self.free_slots.pop_front() {
            // if we have a free index, reuse it
            match self.elements[next_free_index] {
                AllocatorSlot::Free(generation) => {
                    self.elements[next_free_index] = AllocatorSlot::Occupied(generation, value);
                    Key {
                        index: next_free_index,
                        generation,
                    }
                }
                _ => panic!("Tried to insert into a non-empty slot"),
            }
        } else {
            // otherwise extend the vector to hold the new element
            self.elements.push(AllocatorSlot::Occupied(0, value));
            Key {
                index: self.elements.len() - 1,
                generation: 0,
            }
        }
    }

    pub fn get(&self, key: &Key) -> Option<&T> {
        if self.key_is_valid(key) {
            if let AllocatorSlot::Occupied(_, val) = &self.elements[key.index] {
                Some(val)
            } else {
                // This should be unreachable based on our valid key conditions
                None
            }
        } else {
            None
        }
    }

    pub fn get_mut(&mut self, key: &Key) -> Option<&mut T> {
        if self.key_is_valid(key) {
            if let AllocatorSlot::Occupied(_, val) = &mut self.elements[key.index] {
                Some(val)
            } else {
                // This should be unreachable based on our valid key conditions
                None
            }
        } else {
            None
        }
    }

    pub fn free(&mut self, key: &Key) -> Result<T, String> {
        // this will consume the key and remove it from the allocator
        // If it cannot remove the  index, the key is returned as an error
        // TODO: Find a better way to clean this up - returning Err(key) is odd
        if self.key_is_valid(key) {
            // initialize to_return with the value we will swap into our allocator array
            let mut to_return = AllocatorSlot::Free(key.generation + 1);
            mem::swap(&mut to_return, &mut self.elements[key.index]); // swap it out so we retrieve the value in to_return
            self.free_slots.push_back(key.index); // put the slot into our free slot pool
            match to_return{
                AllocatorSlot::Occupied(_,val) => Ok(val),
                _ => unreachable!()
            }
        } else {
            Err(String::from("Could not Free Slot"))
        }
    }

    pub fn next_available_key(&self) -> Key {
        if let Some(next_open_slot) = self.free_slots.front(){
            match self.elements[*next_open_slot]{
                AllocatorSlot::Free(generation) => Key{index: *next_open_slot, generation},
                AllocatorSlot::Occupied(_,_) => panic!("Tried Allocating into an occupied slot")
            } 
        } else {
            Key{ index: self.elements.len(), generation: 0 }
        }
    }

    pub fn iter_values(&self) -> impl Iterator<Item = &T> {
        self.elements.iter().filter_map(|x| match x {
            AllocatorSlot::Occupied(_, v) => Some(v),
            _ => None,
        })
    }

    pub fn iter_keys(&self) -> impl Iterator<Item = Key> + '_ {
        self.elements
            .iter()
            .enumerate()
            .filter_map(|(i, x)| match x {
                AllocatorSlot::Occupied(gen, _) => Some(Key {
                    index: i,
                    generation: *gen,
                }),
                _ => None,
            })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_allocator_insertion() {
        // creating a new allocator and allocating a single cell returns the zero index
        let mut allocator = Allocator::new();
        for i in 0..3 {
            assert_eq!(
                allocator.insert(2 * i),
                Key {
                    index: i,
                    generation: 0
                }
            )
        }
    }

    #[test]
    fn test_allocator_getter() {
        let mut allocator = Allocator::new();
        let key = allocator.insert(4.5);

        // assert a valid key works
        assert_eq!(allocator.get(&key), Some(&4.5));

        // an invalid generation returns None
        assert_eq!(
            allocator.get(&Key {
                index: 0,
                generation: 3
            }),
            None
        );

        // assert an invalid length returns none
        assert_eq!(
            allocator.get(&Key {
                index: 100,
                generation: 0
            }),
            None
        );
    }

    #[test]
    fn test_mutable_get() {
        let original_value = 4.5;
        let new_value = 47.5;
        let mut allocator = Allocator::new();
        let key = allocator.insert(original_value);

        if let Some(val) = allocator.get_mut(&key) {
            *val = new_value;
        }

        assert_eq!(allocator.get(&key), Some(&new_value));
    }

    #[test]
    fn test_slot_reuse() {
        let original_value = 4.5;
        let new_value = 47.5;
        let mut allocator = Allocator::new();
        let key = allocator.insert(original_value);
        allocator.free(&key).unwrap(); // free the key so we can reuse the slot
        let key = allocator.insert(new_value);

        // assert the new value is stored
        assert_eq!(allocator.get(&key), Some(&new_value));

        // assert that the key has increased its generation by one
        assert_eq!(
            key,
            Key {
                generation: 1,
                index: 0
            }
        );
    }

    #[test]
    fn test_iteration() {
        let mut allocator = Allocator::new();
        // fill it with some values
        let keys: Vec<Key> = (0..10).map(|x| allocator.insert(x)).collect();

        // iterate over all the values and make sure they match
        let values_match = (0..10)
            .zip(allocator.iter_values())
            .map(|(x, y)| x == *y)
            .fold(true, |prev, cur| prev && cur);
        assert!(values_match);

        // now free the key at index 4 and make sure it's skipped
        allocator.free(&keys[3]).unwrap();
        let values_match = [0, 1, 2, 4, 5, 6, 7, 8, 9]
            .iter()
            .zip(allocator.iter_values())
            .map(|(x, y)| *x == *y)
            .fold(true, |prev, cur| prev && cur);
        assert!(values_match);
    }

    #[test]
    fn test_key_peek() {
        let mut alloc = Allocator::<usize>::new(); // make an empty allocator
        assert_eq!(alloc.next_available_key(), Key{index: 0, generation: 0}); // our next available key should be the zero index
        
        // push some elements back into the allocator
        let keys: Vec<Key> = (0..3).map(|element| alloc.insert(element)).collect();
        assert_eq!(alloc.next_available_key(), Key{index: 3, generation: 0});
        
        // pop an element and validate the next available key
        alloc.free(&keys[0]).unwrap();
        assert_eq!(alloc.next_available_key(), Key{index: 0, generation: 1});


    }
}
