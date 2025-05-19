mod reverse;
mod students;
mod birds;

fn main() {
    // Reverse a string
    let original = "hello";
    let reversed = reverse::reverse(original);
    println!("Original: {}", original);
    println!("Reversed: {}", reversed);

    //  Count oldest students
    let student_data = "
Alice Cooper 25
Alice Boa 23
Bob Marley 23
Alice Chains 25
Charlie Brown 21
Charlie Chaplin 25
Eve Wonder 24
Sandra White 21
";
    let oldest_count = students::count_oldest_students(student_data);
    println!("Number of oldest students: {}", oldest_count);

    // 3. Bird counter
    let log = vec![
        "AA", "BB", "AA", "CC", "BB", "AA", "BB", "DD", "EE", "BB", "FF", "GG", "BB"
    ];

    let most_frequent_count = birds::find_most_frequent_bird_count(&log);
    println!("Most frequent bird count: {}", most_frequent_count);

    if let Some(bird) = birds::find_most_frequent_bird_no_order(&log) {
        println!("Most frequent bird (any order): {}", bird);
    }

    if let Some(bird) = birds::find_most_frequent_bird(&log) {
        println!("Most frequent bird (first seen wins): {}", bird);
    }
}
