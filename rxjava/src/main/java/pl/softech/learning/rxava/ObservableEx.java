package pl.softech.learning.rxava;

import rx.Observable;

import java.util.Arrays;
import java.util.concurrent.TimeUnit;

import static pl.softech.learning.rxava.Utils.sleep;
import static pl.softech.learning.rxava.Utils.subscribePrint;
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
        observableRange();
        observableInterval();
        observableTimer();
        sleep(1000);
    }

    private static void observableTimer() {
        title("observable timer");
        subscribePrint(Observable.timer(100, TimeUnit.MILLISECONDS),
                "timer");
    }

    private static void observableInterval() {
        title("observable interval");
        subscribePrint(Observable.interval(300, TimeUnit.MILLISECONDS),
                "interval");
    }

    private static void observableError() {
        title("observable error");
        subscribePrint(Observable.error(new IllegalStateException("Illegal")), "error");
    }

    private static void observableEmpty() {
        title("observable empty");
        subscribePrint(Observable.empty(), "empty");
    }

    private static void observableNever() {
        title("observable never - emit nothing & never terminates");
        subscribePrint(Observable.never(), "never");
    }

    private static void observableRange() {
        title("observable range");
        subscribePrint(Observable.range(1, 5),
                "range");
    }

    private static void observableJust() {
        title("observable just");
        subscribePrint(Observable.just("just1", "just2", Arrays.asList("just3", "just4")),
                "just");
    }

    private static void observableFrom() {
        title("observable from");
        subscribePrint(Observable.from(Arrays.asList(1, 10, 100, 1000)), "from");
    }

    private static void simpleUsage() {
        title("simple usage");
        subscribePrint(Observable.unsafeCreate(s -> {
            for (int i = 0; i < 7; i++) {
                s.onNext(i);
            }
            s.onCompleted();
        }), "simple");

    }

}
