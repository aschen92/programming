/*
 * Created by SharpDevelop.
 * User: Aaron
 * Date: 11/2/2011
 * Time: 11:23 PM
 * 
 * To change this template use Tools | Options | Coding | Edit Standard Headers.
 */

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
