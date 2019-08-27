package argparse;

import java.util.*;

/**
 * Parser builder
 */
public class Parser {
    Optional<String> desc = Optional.empty();
    List<Option> positionals = new ArrayList<Option>();

    /**
     * Create Parser w/ description
     *
     * @param desc description
     */
    public Parser(String desc) {
        this.desc = Optional.of(desc);
    }

    /**
     * create Parser w/o description
     */
    public Parser() {

    }

    /**
     * Parses with given options
     *
     * @param argv command line arguments
     * @return mapping of Option.destination to Argument
     */
    public Map<String, Argument> parse(String[] argv) {
        return new HashMap<String, Argument>();
    }
}
