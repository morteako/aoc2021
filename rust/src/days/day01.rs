use std::collections::HashMap;
use std::fs;

pub fn run() {
    let content =
        fs::read_to_string("../inputs/2015/1").expect("Something went wrong reading the file");
    let nums: Vec<i32> = content.lines().map(|x| x.parse::<i32>().unwrap()).collect();

    let mut map: HashMap<i32, i32> = HashMap::with_capacity(nums.len());

    for num in &nums {
        map.insert(2020 - num, *num);
    }

    for num in &nums {
        match map.get(&num) {
            Some(a) => {
                println!("{}", a * num);
                break;
            }
            None => {}
        }
    }

    let mut map2: HashMap<i32, (i32, i32)> = HashMap::with_capacity(nums.len());

    for num in &nums {
        for num2 in &nums {
            map2.insert(2020 - num - num2, (*num, *num2));
        }
    }

    for num in nums {
        match map2.get(&num) {
            Some((a, b)) => {
                println!("{}", a * b * num);
                break;
            }
            None => {}
        }
    }
}
