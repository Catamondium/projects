package argparse;

import java.util.*;
import java.util.stream.Stream;

/*
 * parsing notes:
 *  w/o rest param '--' delimiter fails on following arg
 */

/**
 * Parser builder
 */
public class Parser {
    Optional<String> desc = Optional.empty();
    List<Option> positionals = new ArrayList<Option>();
    List<Option> keywords = new ArrayList<Option>();
    Optional<Option> rest = Optional.empty();

    /**
     * Create Parser w/ description
     *
     * @param desc description
     */
    public Parser(String desc) {
        this.desc = Optional.of(desc);
    }

    /**
     * Create Parser w/o description
     */
    public Parser() {

    }

    /**
     * Add a sequence of options to Parser
     *
     * @param options options
     * @return this
     */
    public Parser addOptions(Option... options) {
        for (var o : options) {
            if (o.arity.mod == ArityMod.REMAINDER) {
                if (!rest.isPresent()) {
                    rest = Optional.of(o);
                } else {
                    // Acknowledge no further remainders
                    continue;
                }
            }

            if (o.isKeyword()) {
                keywords.add(o);
            } else {
                positionals.add(o);
            }
        }
        return this;
    }

    /**
     * Parses with given options
     *
     * @param argv command line arguments
     * @return mapping of Option.destination to Argument
     */
    public ArgResult parse(String[] argv) {
        return new ArgResult();
    }
}
