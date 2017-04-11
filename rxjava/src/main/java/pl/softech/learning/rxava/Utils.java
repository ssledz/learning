package pl.softech.learning.rxava;

import rx.Observer;

/**
 * @author ssledz
 * @since 11.04.17
 */
public final class Utils {

    private Utils() {
    }

    public static void title(String title) {
        System.out.println(String.format("\n\n%s\n", title));
    }

    public static <T> Observer<T> createGenericObserver() {
        return new Observer<T>() {

            @Override
            public void onCompleted() {
                System.out.println("on Completed");
            }

            @Override
            public void onError(Throwable e) {
                System.out.println("on Error");
            }

            @Override
            public void onNext(T item) {
                System.out.println(String.format("Item is %s", item));
            }
        };
    }
}
