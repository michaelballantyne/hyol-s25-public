use ascent::ascent;
use itertools::Itertools;


#[derive(Clone, Debug, Eq, PartialEq, Hash)]
struct Course {
    code: String,
    name: String,
    credits: u32,
}

ascent! {
    relation offered(Course);
    relation prereq(Course, Course);
    relation completed(Course);
    relation can_take(Course);
    
    can_take(c) <-- 
        offered(c),
        !completed(c),
        !blocked_by_prereq(c);
        
    relation blocked_by_prereq(Course);
    blocked_by_prereq(c) <--
        prereq(c, p),
        !completed(p);
}

fn main() {
    // Create course instances
    let cs101 = Course {
        code: "CS101".to_string(),
        name: "Intro to Programming".to_string(),
        credits: 3,
    };
    
    let cs201 = Course {
        code: "CS201".to_string(),
        name: "Data Structures".to_string(),
        credits: 4,
    };
    
    let cs301 = Course {
        code: "CS301".to_string(),
        name: "Algorithms".to_string(),
        credits: 4,
    };

    let mut prog = AscentProgram::default();
    
    prog.offered = vec![
        (cs101.clone(),),
        (cs201.clone(),),
        (cs301.clone(),)
    ];
    
    prog.prereq = vec![
        (cs201.clone(), cs101.clone()),
        (cs301.clone(), cs101.clone())
    ];
    
    prog.completed = vec![(cs101.clone(),)];
    
    prog.run();

    let schedules = prog.can_take.iter().map(|c| c.0.clone()).combinations(2);

    println!("Schedules:");
    for schedule in schedules {
      println!("{:?}", schedule);
    }
}