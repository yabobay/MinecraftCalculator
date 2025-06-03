package MinecraftCalculator;

import org.bukkit.plugin.java.JavaPlugin;
import org.bukkit.command.*;
import java.util.logging.Logger;

public final class Main extends JavaPlugin {
    @Override
    public boolean onCommand(CommandSender sender, Command cmd, String label, String[] args) {
	Logger log = getLogger();
	// i don't know the difference between this and label but whatever
	if (cmd.getName().equalsIgnoreCase("calc")) {
	    if (args.length == 0)
		return false;
	    String math = String.join(" ", args);
	    String response = "[calc] " + eval(math);
	    sender.sendMessage(response);
	    return true;
	}
	return false;
    }

    String eval(String math) {
	return Math.apply(math);
    }
}
