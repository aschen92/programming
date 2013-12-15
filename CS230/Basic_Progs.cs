/*using System;

class Program
{
    static void Main()
    {
	// Convert string to number.
	string text = "500";
	int num = int.Parse(text);
	Console.WriteLine(num);
    }
}
 */

using System;

class Program
{
    static void Main()
    {
	while (true) // Loop indefinitely
	{
	    Console.WriteLine("Enter input:"); // Prompt
	    string line = Console.ReadLine(); // Get string from user
	    if (line == "exit") // Check string
	    {
		break;
	    }
	    Console.Write("You typed "); // Report output
	    Console.Write(line.Length);
	    Console.WriteLine(" character(s)");
	}
    }
}





/*using System;

class futval
{
	static void Main()
	{
		string principalstr;
		string aprstr;
		Console.WriteLine("This program calculates the future value \n of a 10-year investment.");
		
		Console.WriteLine("Enter the initial principal: ");
		
		principalstr = "32"; //Console.ReadLine();
		int principal = int.Parse(principalstr);
		
		Console.WriteLine("Enter the annual interest rate: ");
		aprstr = "2";//Console.ReadLine();
		int apr = int.Parse(aprstr);
		
		for (int n=10; n<10; n++) {
			Console.WriteLine("kitty");
			principal = principal * (1 + apr);
		}
		
		Console.WriteLine("The value in 10 years is: ", principal);
	}
}
 */




/*public class Recursion{
  
  public static long fact(int n){
    if(n==0){
      return 1;
    }
    else{
      return n * fact(n-1);
    }
  }






using System;
 
class Program
{
    static void Main()
    {
        Console.WriteLine("Hello, World!");
    }
}*/