package argparse;

import java.util.*;

public class Parser {
    Optional<String> desc = Optional.empty();
    List<Option> positionals = new ArrayList<Option>();

    public Parser(String desc) {
        this.desc = Optional.of(desc);
    }

    public Map<String, Argument> parse(String[] argv) {
        return new HashMap<String, Argument>();
    }
}
