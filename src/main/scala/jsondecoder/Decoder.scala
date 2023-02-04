package jsondecoder

trait Decoder[S, R] {
    self =>
    def apply(src: S): Option[R]
    def map[R1](f: R => R1): Decoder[S, R1] = new Decoder[S, R1] {
        def apply(src: S): Option[R1] = self(src).map(f)
    }
    def flatMap[R1](f: R => Option[R1]): Decoder[S, R1] = new Decoder[S, R1] {
        def apply(src: S): Option[R1] = self(src).flatMap(f)
    }
    def filter(predicate: R => Boolean): Decoder[S, R] = new Decoder[S, R] {
        def apply(src: S): Option[R] = self(src).filter(predicate)
    }
    def collect[R1](f: PartialFunction[R, R1]): Decoder[S, R1] = new Decoder[S, R1] {
        def apply(src: S): Option[R1] = self(src).collect(f)
    }
}
