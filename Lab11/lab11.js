
class Student {
    constructor(firstName,lastName,studentID) {
        this.firstName = firstName;
        this.lastName = lastName;
        this.studentID = studentID;
    }

    display() {
        console.log(this.firstName,this.lastName,this.studentID);
    }
}

//Create array of students, with one of them graduated

studentArray = [new Student("Anant","Shukla","010000001"),
new Student("Prayuj","Pillai","010000002"),
new Student("SaiReddy","Vaka","010000003")]

studentArray[0].graduated=true;

//Create a new student without using above constructor
fakeStudent = {"firstName":"first","lastName":"last","studentID":"000000090"}
fakeStudent.__proto__.display=studentArray[0].display;

//Test display method for each student
studentArray[0].display();
studentArray[1].display();
studentArray[2].display();
fakeStudent.display();