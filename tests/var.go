package main;
import "fmt";

func f(a int) (int, int) {
	fmt.Print(a);
}

// Résultat attendu 42
func main() {
     var x, y int;
       x = 1;
       y = 6;
       x = x+2;
       y = y*(x+4);
       fmt.Print(y)
};
