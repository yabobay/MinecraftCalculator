package MinecraftCalculator;

public class Cli {
    public static void main(String[] args) throws Math.SyntaxError {
	if (args.length == 0) {
	    System.out.println("Please give input");
	    System.exit(0);
	}
	String math = String.join(" ", args);
	System.out.printf("Expression: %s\n", math);
	System.out.println(Math.apply(math, true));
    }
}
