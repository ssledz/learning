package pl.softech.learning.rxava;

import rx.Observable;

import java.util.Arrays;

import static pl.softech.learning.rxava.Utils.createGenericObserver;
import static pl.softech.learning.rxava.Utils.title;

/**
 * @author ssledz
 * @since 11.04.17
 */
public class ObservableEx {


    public static void main(String[] args) {

        simpleUsage();
        observableFrom();
        observableJust();
        observableNever();
        observableEmpty();
        observableError();

    }

    private static void observableError() {
        title("observableError");
        Observable.error(new IllegalStateException())
                .subscribe(createGenericObserver());
    }

    private static void observableEmpty() {
        title("observableEmpty");
        Observable.empty()
                .subscribe(createGenericObserver());
    }

    private static void observableNever() {
        title("observableNever - emit nothing & never terminates");
        Observable.never()
                .subscribe(createGenericObserver());
    }

    private static void observableJust() {
        title("observableJust");
        Observable.just("just1", "just2", Arrays.asList("just3", "just4"))
                .subscribe(createGenericObserver());
    }

    private static void observableFrom() {
        title("observableFrom");
        Observable.from(Arrays.asList(1, 10, 100, 1000))
                .subscribe(createGenericObserver());
    }

    private static void simpleUsage() {
        title("simpleUsage");
        Observable<Integer> observable = Observable.unsafeCreate(s -> {
            for (int i = 0; i < 7; i++) {
                s.onNext(i);
            }
            s.onCompleted();
        });

        observable.subscribe(createGenericObserver());
    }

}
