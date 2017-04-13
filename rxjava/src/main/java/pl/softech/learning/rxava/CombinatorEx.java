package pl.softech.learning.rxava;

import rx.Observable;

import java.util.concurrent.TimeUnit;

import static pl.softech.learning.rxava.Utils.*;

/**
 * @author ssledz
 * @since 12.04.17
 */
public class CombinatorEx {

    public static void main(String[] args) {
        zipEx();
        mergeEx();
        joinEx();

        sleep(1000);
    }

    private static void joinEx() {
        title("join combinator");
        Observable<Long> i1 = Observable.interval(300, TimeUnit.MILLISECONDS);
        Observable<Long> i2 = Observable.interval(300, TimeUnit.MILLISECONDS);

        Observable<Long> span1 = Observable.timer(600, TimeUnit.MILLISECONDS);
        Observable<Long> span2 = Observable.timer(0, TimeUnit.MILLISECONDS);

        Observable<String> ret = i1.join(i2, i -> span1, i -> span2, (ii1, ii2) -> String.format("%d-%d", ii1, ii2));
        subscribePrint(ret, "join");
    }

    private static void mergeEx() {
        title("zip combinator");
        Observable<String> ob1 = Observable.just("a", "b", "c", "d");
        Observable<String> ob2 = Observable.just("1", "2", "3");
        subscribePrint(ob1.mergeWith(ob2), "merge");
    }

    private static void zipEx() {
        title("zip combinator");
        Observable<String> ob1 = Observable.just("a", "b", "c", "d");
        Observable<Integer> ob2 = Observable.just(1, 2, 3);
        subscribePrint(ob1.zipWith(ob2, (a, b) -> String.format("%s%s", a, b)),
                "zip");
    }
}
