import argparse.*;

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
        Parser parser = new Parser("time arithmetic utility");
    }
}