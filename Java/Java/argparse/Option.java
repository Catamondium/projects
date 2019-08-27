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

/**
 * Option builder
 */
class Option {
    boolean keyword = false;
    String smallname;
    String destination;
    OptArity arity = new OptArity();

    Optional<String> longname = Optional.empty();
    Optional<String> desc = Optional.empty();

    /**
     * Constructs keyword or positional argument
     *
     * @param name given name of Option
     */
    public Option(String name) {
        this.smallname = name;
        if (smallname.startsWith("-")) {
            keyword = true;
            destination = name.substring(1);
        } else {
            destination = name;
        }
    }

    /**
     * Constructs keyword argument
     *
     * @param smallname small form e.g "-h"
     * @param longname  long form e.g "--help"
     */
    public Option(String smallname, String longname) {
        String[] strs = dashize(smallname, longname);
        this.smallname = strs[0];
        this.longname = Optional.of(strs[1]);
        destination = strs[2];
        keyword = true;
    }

    /**
     * Sets Option arity
     *
     * @param n   required args / lower bound
     * @param mod special arities e.g variadic
     * @return
     */
    public Option arity(int n, ArityMod mod) {
        arity = new OptArity(n, mod);
        return this;
    }

    /**
     * Equivalent to arity(n, ArityMod.FIXED), sets n-ary requirement
     *
     * @param n required args
     * @return this
     */
    public Option arity(int n) {
        arity = new OptArity(n);
        return this;
    }

    /**
     * sets nullary arity, i.e boolean flag
     *
     * @return this
     */
    public Option arity() {
        arity = new OptArity();
        return this;
    }

    /**
     * sets Option description
     *
     * @param desc description
     * @return this
     */
    public Option description(String desc) {
        this.desc = Optional.of(desc);
        return this;
    }

    /**
     * Sets destination key, defaults to longname else smallname
     *
     * @param dest key
     * @return this
     */
    public Option destination(String dest) {
        destination = dest;
        return this;
    }

    /**
     *
     * @return option keyword status
     */
    public boolean isKeyword() {
        return keyword;
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
        } else if (longname.startsWith("-")) {
            ret[1] = "-" + longname;
        } else {
            ret[1] = "--" + longname;
        }

        ret[2] = ret[1];

        return ret;
    }
}