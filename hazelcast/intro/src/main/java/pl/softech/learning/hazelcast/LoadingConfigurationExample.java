package pl.softech.learning.hazelcast;

import com.hazelcast.config.ClasspathXmlConfig;
import com.hazelcast.config.Config;
import com.hazelcast.config.FileSystemXmlConfig;
import com.hazelcast.core.Hazelcast;
import com.hazelcast.core.HazelcastInstance;

import java.io.FileNotFoundException;

/**
 * @author ssledz
 * @since 24.03.16
 */
public class LoadingConfigurationExample {


    public static void main(String[] args) throws FileNotFoundException {

        loadingDefaultConfiguration();
//        loadingConfigurationFromClassPath();
//        loadingConfigurationFromFileSystem();
//        loadingConfigurationProgrammatically();

        System.exit(0);
    }

    private static void loadingConfigurationProgrammatically() {
        Config config = new Config();
        config.setInstanceName("myCache")
                .getNetworkConfig()
                .getJoin()
                .getMulticastConfig()
                .setEnabled(true);
        HazelcastInstance hz = Hazelcast.newHazelcastInstance(config);
    }

    private static void loadingConfigurationFromFileSystem() throws FileNotFoundException {
        Config config = new FileSystemXmlConfig("src/main/resources/hazelcast.xml");
        HazelcastInstance hz = Hazelcast.newHazelcastInstance(config);
    }

    private static void loadingConfigurationFromClassPath() {
        Config config = new ClasspathXmlConfig("hazelcast.xml");
        HazelcastInstance hz = Hazelcast.newHazelcastInstance(config);
    }

    private static void loadingDefaultConfiguration() {
        HazelcastInstance hz = Hazelcast.newHazelcastInstance();
    }

}
