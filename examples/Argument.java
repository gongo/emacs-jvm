class Argument
{
    public static void main(String[] args)
    {
        int a1 = Integer.parseInt(args[0]);
        int a2 = Integer.parseInt(args[1]);
        System.out.printf("a1 (%d) + a2 (%d) = %d\n", a1, a2, a1 + a2);
    }
}
