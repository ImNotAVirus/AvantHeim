use pathfinding::prelude::{absdiff, astar};
use rustler::{Atom, Binary};

mod atoms {
    rustler::atoms! {
        // Errors
        no_path,
    }
}

type Pos = (i32, i32);
static WALKABLE_CELL: u8 = 0;
static NEIGHBORS_OFFSETS: [Pos; 8] = [
    (0, 1),
    (1, 0),
    (0, -1),
    (-1, 0),
    (1, 1),
    (1, -1),
    (-1, -1),
    (-1, 1),
];

#[rustler::nif(name = "astar", schedule = "DirtyCpu")]
fn astar_path(
    map: Binary,
    width: i32,
    height: i32,
    start: Pos,
    end: Pos,
) -> Result<Vec<Pos>, Atom> {
    let result = astar(
        // start
        &start,
        // neightbors
        |&pos| get_neightbors(map, width, height, &pos),
        // distance
        |&(x, y)| absdiff(x, end.0) + absdiff(y, end.1),
        // end
        |&p| p == end,
    );

    match result {
        Some((path, _total_cost)) => Ok(path),
        None => Err(atoms::no_path()),
    }
}

fn get_neightbors(map: Binary, width: i32, height: i32, (x, y): &Pos) -> Vec<(Pos, i32)> {
    NEIGHBORS_OFFSETS
        .iter()
        .map(|(offset_x, offset_y)| {
            let new_x = x + offset_x;
            let new_y = y + offset_y;

            if new_x < 0 || new_y < 0 || new_x >= width || new_y >= height {
                None
            } else if map[(new_y * width + new_x) as usize] != WALKABLE_CELL {
                None
            } else {
                Some(((new_x, new_y), 1))
            }
        })
        .filter_map(|x| x)
        .collect()
}

rustler::init!("Elixir.Algorithms.Pathfinding", [astar_path]);
