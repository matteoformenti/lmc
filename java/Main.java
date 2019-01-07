import java.io.BufferedReader;
import java.io.FileReader;

class LMC {
    String[] instructions;
    int[] compiled;
    HashMap<String, Integer> labels;

  lmcLoad(String filename) {
    BufferedReader reader = new BufferedReader(new FileReader(filename));
    String line = reader.readLine();

    while (line != null)

  }

  lmcRun(String filename, int[] input) { lmcLoad(filename); }
}