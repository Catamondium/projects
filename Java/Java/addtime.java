import java.util.Arrays;
import java.util.regex.*;
import java.util.Vector;
import java.util.List;

class Time {
    public int hrs = 0;
    public int mins = 0;

    public Time(String strTime) {
        if (strTime.contains(":")) {
            String[] parts = strTime.split(":");
            this.hrs = Integer.parseInt(parts[0]);
            this.mins = Integer.parseInt(parts[1]);
        } else {
            int mins = Integer.parseInt(strTime);
            this.hrs = Math.floorDiv(mins, 60);
            this.mins = Math.floorMod(mins, 60);
        }
    }

    public Time(int hrs, int mins) {
        this.hrs = hrs;
        this.mins = mins;
    }

    public Time(int mins) {
        this.hrs = Math.floorDiv(mins, 60);
        this.mins = Math.floorMod(mins, 60);
    }

    public Time elapse(int e) {
        return new Time(this.abs() + e);
    }

    public Time elapse(Time other) {
        return new Time(this.abs() + other.abs());
    }

    public int abs() {
        return (this.hrs * 60) + this.mins;
    }

    public String toString() {
        return String.format("%02d:%02d", this.hrs, this.mins);
    }
}

class Argparse {
    public static Pattern optRe = Pattern.compile("^-[-a-zA-Z]+");
    public List<String> opts = new Vector<String>();
    public List<String> free = new Vector<String>();

    Argparse(String[] argv) {
        for (String s : argv) {
            Matcher m = optRe.matcher(s);
            if (m.matches()) {
                this.opts.add(s);
            } else {
                this.free.add(s);
            }
        }
    }

    public boolean hasOpt(String opt) {
        return (opts.indexOf(opt) != -1);
    }
}

class addtime {
    public static void usage() {
        StringBuilder sb = new StringBuilder();
        sb.append("Usage: [-hql] <HH:MM> <mins | HH:MM>\n");
        sb.append("options:\n\t-q quietly output end time\n");
        sb.append("\t-h print this message and exit\n");
        sb.append("\t-l print written language");
        System.out.println(sb);
        System.exit(1);
    }

    public static void main(String[] argv) {
        Argparse args = new Argparse(argv);

        if (args.hasOpt("-h")) {
            usage();
        } else if (args.hasOpt("-l")) {
            System.out.format("Java: %s\n", System.getProperty("java.version"));
            System.exit(0);
        } else if (args.free.size() < 2)
            usage();

        Time start = new Time(args.free.get(0));
        Time elapse = new Time(args.free.get(1));
        Time end = start.elapse(elapse);

        if (args.hasOpt("-q")) {
            System.out.println(end);
        } else {
            System.out.format("Start:\t%s\t%+d\n", start, elapse.abs());
            System.out.format("End:\t%s\n", end);
        }
    }
}
