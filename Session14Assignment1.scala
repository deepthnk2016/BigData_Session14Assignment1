object Rationals {

 def main(args: Array[String]) {

 // Case 1: Operation on two rationals
 //Set the first rational number / integer 
// var firstNumer:Int = 4
// var firstDenom:Int = 3

 //Set the second rational number / integer
 //var secondNumer:Int = 10
 //var secondDenom:Int = 4



 // Case 2: Operation on two integers 
 //var firstNumer:Int = 4
 //var firstDenom:Int = 2

 //Set the second rational number / integer
 //var secondNumer:Int = 10
 //var secondDenom:Int = 5


 // Case 3: Operation on one rational and one integers 
 var firstNumer:Int = 4
 var firstDenom:Int = 2

 //Set the second rational number / integer
 var secondNumer:Int = 10
 var secondDenom:Int = 3
 
 // Now, pass the above numbers to Rational Class
 var a = new Rational(firstNumer,firstDenom)
 var b = new Rational(secondNumer,secondDenom)

 // Get the Result
 var result=a+b

 // Print the Result - don't print the denominator if it is 1
 if (result.denom==1) println("Result of " + a.numer + " + "+b.numer+" is :"+result.numer)
 else  if (a.denom!=1 && b.denom!=1)println("Result of "+a.numer+"/"+a.denom+" + "+ b.numer +"/"+b.denom+" is: " + result.numer + "/" + result.denom)
  else  if (a.denom==1 && b.denom!=1)println("Result of "+a.numer+"+"+b.numer+"/"+b.denom+" is: " + result.numer + "/" + result.denom)
 else println("Result of "+a.numer+"/"+a.denom+" + "+ b.numer + "is: " + result.numer + "/" + result.denom)
}
	
}

class Rational(n: Int, d: Int) {
//GCD method
private def gcd(x: Int, y: Int): Int = {
if (x == 0) y
else if (x < 0) gcd(-x, y)
else if (y < 0) -gcd(x, -y)
else gcd(y % x, x)
}
// Auxillary Constructor
def this(x:Int){
 this(x,1)
}
private val g = gcd(n, d)
val numer: Int = n/g
val denom: Int = d/g


//Method Overloading for each type of operation
//Addition
def +(that: Rational) = new Rational(numer*that.denom + that.numer*denom,denom*that.denom)

//Subtraction
def -(that: Rational) = new Rational(numer*that.denom - that.numer*denom,denom*that.denom)

//Multiplication
def *(that: Rational) = new Rational(numer*that.numer, denom*that.denom)

//Division
def /(that: Rational) = new Rational(numer*that.denom, denom*that.numer)

}
