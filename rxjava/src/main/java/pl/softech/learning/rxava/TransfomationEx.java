package pl.softech.learning.rxava;

import rx.Observable;

import static pl.softech.learning.rxava.Utils.subscribePrint;
import static pl.softech.learning.rxava.Utils.title;

/**
 * @author ssledz
 * @since 13.04.17
 */
public class TransfomationEx {

    public static void main(String[] args) {

        flatMap();

    }

    private static void flatMap() {
        title("flatMap transformation");

        Observable<Integer> flatMapped = Observable
                .just(5, 432)
                .flatMap(
                        v -> Observable.range(v, 2),
                        (x, y) -> x + y);
        subscribePrint(flatMapped, "flatMap");
    }

}
