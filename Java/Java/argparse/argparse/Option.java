package argparse;

import java.util.Optional;

class OptArity {
    int n = 0; // 0 signals presence-boolean
    ArityMod mod = ArityMod.FIXED;

    OptArity(int n, ArityMod mod) {
        this.mod = mod;
        switch (mod) {
        case REST:
        case GREEDY:
            this.n = -1; // -1 signals irrelevance
            break;
        case FIXED:
            this.n = Math.abs(n);
            break;
        case PLUS:
            this.n = Math.abs(n);
            break;
        default:
            break;
        }
    }

    OptArity() {

    }

    OptArity(int n) {
        this.n = n;
    }
}

class Option {
    boolean positional = false;
    String smallname;
    Optional<String> longname = Optional.empty();
    String assumed_name;
    OptArity arity = new OptArity();

    public Option(String smallname) {
        this.smallname = smallname;
        if (smallname.startsWith("-")) {
            positional = true;
            assumed_name = smallname.substring(1);
        } else {
            assumed_name = smallname;
        }
    }

    public Option(String smallname, String longname) {
        String[] strs = dashize(smallname, longname);
        this.smallname = strs[0];
        this.longname = Optional.of(strs[1]);
        assumed_name = strs[2];
    }

    public Option arity(int n, ArityMod m) {
        arity = new OptArity(n, m);
        return this;
    }

    public Option arity(int n) {
        arity = new OptArity(n);
        return this;
    }

    private String[] dashize(String smallname, String longname) {
        String[] ret = { "", "", "" };
        if (smallname.startsWith("-")) {
            ret[0] = smallname;
        } else {
            ret[0] = "-" + smallname;
        }

        if (longname.startsWith("--")) {
            ret[1] = longname;
            ret[2] = longname.substring(1);
        } else if (longname.startsWith("-")) {
            ret[1] = "-" + longname;
            ret[2] = longname.substring(2);
        } else {
            ret[1] = "--" + longname;
            ret[2] = longname;
        }
        return ret;
    }
}