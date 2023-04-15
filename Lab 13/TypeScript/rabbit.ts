class Rabbit {
    name: string;
    constructor(name: string) {
      this.name = name;
    }
  }
  
  const name = "Monty";
  const r = new Rabbit("Python");
  
  console.log(r.name);  // Prints "Python"
  console.log(name);    // Prints "Monty"

  export {}
  