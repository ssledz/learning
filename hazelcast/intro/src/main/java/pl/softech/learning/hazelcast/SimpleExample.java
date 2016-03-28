package pl.softech.learning.hazelcast;

import com.hazelcast.core.Hazelcast;
import com.hazelcast.core.HazelcastInstance;
import com.hazelcast.core.IAtomicReference;

import java.util.Random;

/**
 * @author ssledz
 * @since 24.03.16
 */
public class SimpleExample {

    public static void main(String[] args) throws InterruptedException {

        HazelcastInstance hz = Hazelcast.newHazelcastInstance();

        IAtomicReference<String> ref = hz.getAtomicReference("reference");
        Random rnd = new Random();
        ref.alter(s -> "foo" + rnd.nextInt(100));
        while (true) {
            System.out.println(ref.get());
            Thread.sleep(1000);
        }

    }

}
