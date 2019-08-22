import java.lang.Math;

class Time {
    public int hrs;
    public int mins;

    public Time(String strTime) {
        String[] parts = strTime.split(":");
        this.hrs = Integer.parseInt(parts[0]);
        this.mins = Integer.parseInt(parts[1]);
    }

    public Time(int hrs, int mins) {
        this.hrs = hrs;
        this.mins = mins;
    }

    public static Time elapse(Time s, int e) {
        int offset = (s.hrs * 60) + s.mins;
        int tot = offset + e;
        return new Time((int) Math.floor(tot / 60), tot % 60);
    }

    public String format() {
        return String.format("%02d:%02d", this.hrs, this.mins);
    }
}

class addtime {
    public static void usage() {
        System.out.println("Usage: <HH:MM> <mins>");
        System.exit(1);
    }

    public static void main(String[] argv) {
        if (argv.length < 2)
            usage();

        Time start = new Time(argv[0]);
        int elapse = Integer.parseInt(argv[1]);
        Time end = Time.elapse(start, elapse);

        String p1 = String.format("Start:\t%s\t%+d", start.format(), elapse);
        String p2 = String.format("End:\t%s", end.format());
        System.out.println(p1);
        System.out.println(p2);
    }
}
