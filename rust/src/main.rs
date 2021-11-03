use std::collections::HashMap;
use std::fs;

fn main() {
    let content = fs::read_to_string("../inputs/1").expect("Something went wrong reading the file");
    let nums = content.lines().map(|x| x.parse::<i32>().unwrap());
    // let nums2 = ;
    let mut map: HashMap<i32, i32> = HashMap::with_capacity(200);
    for num in nums.clone() {
        map.insert(2020 - num, num);
    }
    for num in nums.clone() {
        match map.get(&num) {
            Some(a) => {
                println!("{}", a * num);
                break;
            }
            None => {}
        }
    }

    let mut map2: HashMap<i32, (i32, i32)> = HashMap::with_capacity(200);

    for num in nums.clone() {
        for num2 in nums.clone() {
            map2.insert(2020 - num - num2, (num, num2));
        }
    }

    for num in nums.clone() {
        match map2.get(&num) {
            Some((a, b)) => {
                println!("{}", a * b * num);
                break;
            }
            None => {}
        }
    }
}
