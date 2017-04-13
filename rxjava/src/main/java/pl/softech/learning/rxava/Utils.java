package pl.softech.learning.rxava;

import rx.Observable;
import rx.Observer;

import java.util.stream.Stream;

/**
 * @author ssledz
 * @since 11.04.17
 */
public final class Utils {

    private Utils() {
    }

    public static void sleep(long millis) {
        try {
            Thread.sleep(millis);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

    public static void title(String title) {
        System.out.println();
        System.out.println(title);
        Stream.generate(() -> "-").limit(title.length()).forEach(System.out::print);
        System.out.println();
    }

    public static <T> void subscribePrint(Observable<T> observable, String name) {

        observable.subscribe(new Observer<T>() {
            @Override
            public void onCompleted() {
                System.out.printf("%s completed !\n", name);
            }

            @Override
            public void onError(Throwable e) {
                System.out.printf("Error from %s : %s\n", name, e.getMessage());
            }

            @Override
            public void onNext(T t) {
                System.out.printf("%s : %s\n", name, t);
            }
        });

    }

}
