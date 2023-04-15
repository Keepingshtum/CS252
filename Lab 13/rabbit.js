var name = "Monty";
var r = new Rabbit("Python");


function Rabbit(name) {
    this.name = name;
}


console.log(r.name);  // Prints "Python"
console.log(name);    // Prints "Monty"
