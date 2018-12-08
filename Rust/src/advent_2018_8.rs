
extern crate adventlib;

#[derive(Debug, Clone)]
struct Tree {
    children: Vec<Tree>,
    metadata: Vec<u32>
}

impl Tree {
    fn sum_metadata(&self) -> u32 {
        self.metadata.iter().sum::<u32>()
            + self.children.iter().map(|child| child.sum_metadata()).sum::<u32>()
    }

    fn sum_special(&self) -> u32 {
        if self.children.is_empty() {
            return self.metadata.iter().sum();
        }

        return self.metadata.iter()
            .map(|metadata| {
                let child_index: usize = (*metadata - 1) as usize;
                self.children.get(child_index).map_or(0, |child| child.sum_special())
            })
            .sum()
    }
}

fn read_tree(data: &[u32]) -> Option<(Tree, usize)> {
    let child_quantity = data[0] as usize;
    let metadata_quantity = data[1] as usize;

    let mut read = 2; // We've already read two numbers
    let mut tree = Tree {
        children: Vec::with_capacity(child_quantity),
        metadata: Vec::with_capacity(metadata_quantity)
    };

    for _ in 0..child_quantity {
        let (child, child_size) = read_tree(&data[read..])?;
        tree.children.push(child);
        read += child_size;
    }

    for i in 0..metadata_quantity {
        let metadata = data[read + i];
        tree.metadata.push(metadata);
    }

    read += metadata_quantity;

    return Some((tree, read));
}

fn main() {
    let datums: Vec<u32> = adventlib::read_single_input_line("input.txt")
        .split(" ")
        .map(|s| s.parse().expect("Was not a number!"))
        .collect();

    let (tree, size) = read_tree(&datums).expect("Could not read tree");
    assert_eq!(size, datums.len(), "Looks like we didn't read the whole input");

    println!("P1: {}", tree.sum_metadata());
    println!("P2: {}", tree.sum_special());
}