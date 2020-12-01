import scala.util.chaining._

package object minicheck {
trait Random {
  def next: (Random, Long)
  def split: (Random, Random)
  def left = split._1; def right = split._2 }

final case class SplitMix64(x: Long, gamma: Long) extends Random {
  import SplitMix64._
  def next: (SplitMix64, Long) =
    (SplitMix64(x + gamma, gamma), mix64(x + gamma))
  def split: (SplitMix64, SplitMix64) =
    (SplitMix64(x + gamma * 2, gamma),
    SplitMix64(mix64(x + gamma), mixGamma(x + gamma * 2))) }
object SplitMix64 {
  private val GoldenGamma: Long = 0x9e3779b97f4a7c15L
  def apply(x: Long): SplitMix64 =
    SplitMix64(mix64(x), mixGamma(GoldenGamma + x))
  private def mix64(z0: Long): Long = {
    val z1 = (z0 ^ (z0 >>> 33)) * 0xff51afd7ed558ccdL
    val z2 = (z1 ^ (z1 >>> 33)) * 0xc4ceb9fe1a85ec53L
    z2 ^ (z2 >>> 33) }
  private def mixGamma(z0: Long): Long = {
    val z1 = (z0 ^ (z0 >>> 30)) * 0xbf58476d1ce4e5b9L
    val z2 = (z1 ^ (z1 >>> 27)) * 0x94d049bb133111ebL
    val z3 = (z2 ^ (z2 >>> 31)) | 1
    val n = java.lang.Long.bitCount(z3 ^ (z3 >>> 1))
    if (n >= 24) z3 else z3 ^ 0xaaaaaaaaaaaaaaaaL } }

case class GenQC[A](gen: Random => (Random, A)) {
  def map[B](f: A => B): GenQC[B] =
    GenQC { r => val (r1, x) = gen(r); (r1, f(x)) }
  def product[B](g: GenQC[B]): GenQC[(A, B)] = GenQC { r =>
    val (r1, x) = gen(r); val (r2, y) = g.gen(r1); (r2, (x, y)) } }

def shrinkQC[A](x: A, s: A => LazyList[A], p: A => Boolean): A =
  s(x).find(x => !p(x)) match { case None => x; case Some(y) => shrinkQC(y, s, p) }

def sBoolean(x: Boolean): LazyList[Boolean] =
  if (x) LazyList(false) else LazyList.empty
def sInt(x: Int): LazyList[Int] =
  LazyList.iterate(x / 2)(_ / 2).takeWhile(_ != 0).map(x - _)

def sProduct[A, B](sa: A => LazyList[A],
    sb: B => LazyList[B]): ((A, B)) => LazyList[(A, B)] =
  { case (x, y) => sa(x).map((_, y)) ++ sb(y).map((x, _)) }

case class GenShrink[A](g: GenQC[A], s: A => LazyList[A])

case class Tree[A](x: A, ts: LazyList[Tree[A]] = LazyList.empty) {
  def map[B](f: A => B): Tree[B] = Tree(f(x), ts.map(_.map(f)))
  def product[B](t: Tree[B]): Tree[(A, B)] =
    Tree((x, t.x), ts.map(_.product(t)) ++ t.ts.map(product(_)))
  def expand(s: A => LazyList[A]): Tree[A] = {
    def f(x: A): LazyList[Tree[A]] =
      s(x).map(y => Tree(y, f(y))) ++ ts.map(_.expand(s))
    Tree(x, f(x)) } }
case class Gen[A](gen: Random => (Random, Tree[A])) {
  def map[B](f: A => B): Gen[B] = Gen { r =>
    val (r1, t) = gen(r); (r1, t.map(f)) }
  def product[B](g: Gen[B]): Gen[(A, B)] = Gen { r =>
    val (r1, t) = gen(r); val (r2, u) = g.gen(r1); (r2, t.product(u)) } }
object Gen {
  def from[A](g: GenShrink[A]): Gen[A] = Gen { r =>
    val (r1, x) = g.g.gen(r); (r1, Tree(x).expand(g.s)) } }

def vBoolean(r: Random, x: Boolean): Random = if (x) r.right else r.left
def vInt(r: Random, x: Int): Random = if (x == 0) r.left
  else vInt((if ((x & 1) == 1) r.right else r.left).right, x >>> 1)

def functionGenQC[A, B](v: (Random, A) => Random, g: GenQC[B]): GenQC[A => B] =
  GenQC { r => val (r1, r2) = r.split; (r1, x => g.gen(v(r2, x))._2) }

sealed trait :=>[A, B] {
  def map[C](f: B => C): A :=> C
  def table: LazyList[(A, B)]
  def lift: A => Option[B]}
case class Empty[A, B]() extends (A :=> B) {
  def map[C](f: B => C): A :=> C = Empty[A, C]()
  def table: LazyList[(A, B)] = LazyList.empty
  def lift: A => Option[B] = _ => None }
case class Point[B](y: () => B) extends (Unit :=> B) {
  def map[C](f: B => C): Unit :=> C = Point(() => f(y()))
  def table: LazyList[(Unit, B)] = LazyList(((), y()))
  def lift: Unit => Option[B] = _ => Some(y()) }
case class Choice[A, B, C](ac: A :=> C, bc: B :=> C) extends (Either[A, B] :=> C) {
  def map[D](f: C => D): Either[A, B] :=> D = Choice(ac.map(f), bc.map(f))
  def table: LazyList[(Either[A, B], C)] =
    ac.table.map { case (x, y) => (Left(x), y) } ++
    bc.table.map { case (x, y) => (Right(x), y) }
  def lift: Either[A, B] => Option[C] =
    { case Left(x) => ac.lift(x); case Right(x) => bc.lift(x) } }
case class Uncurry[A, B, C](abc: A :=> (B :=> C)) extends ((A, B) :=> C) {
  def map[D](f: C => D): (A, B) :=> D = Uncurry(abc.map(_.map(f)))
  def table: LazyList[((A, B), C)] = abc.table.flatMap { case (x, bc) =>
    bc.table.map { case (y, z) => ((x, y), z) } }
  def lift: ((A, B)) => Option[C] = { case (x, y) =>
    abc.map(_.lift(y)).lift(x).flatten } }
case class Iso[A, B, C](f: A => B, g: B => A, bc: B :=> C) extends (A :=> C) {
  def map[D](h: C => D): A :=> D = Iso(f, g, bc.map(h))
  def table: LazyList[(A, C)] = bc.table.map { case (y, z) => (g(y), z) }
  def lift: A => Option[C] = x => bc.lift(f(x)) }
case class Fun[A, B](ab: A :=> B, y: B) extends (A => B) {
  def apply(x: A): B = ab.lift(x).getOrElse(y)
  override def toString: String =
    (ab.table.map { case (x, y) => s"case $x => $y" } ++ LazyList(s"case _ => $y"))
      .mkString("{", "; ", "}") }

def sPFun[A, B](s: B => LazyList[B]): (A :=> B) => LazyList[A :=> B] = {
  case Empty() => LazyList.empty
  case Point(y) => LazyList(Empty[A, B]()) ++
    LazyList(y).flatMap(y => s(y()).map(y => Point(() => y)))
  case Choice(ac, bc) =>
    sPFun(s)(ac).map(Choice(_, bc)) ++ sPFun(s)(bc).map(Choice(ac, _))
  case Uncurry(abc) => abc.pipe(sPFun(sPFun(s)(_))).map(Uncurry(_))
  case Iso(f, g, bc) => sPFun(s)(bc).map(Iso(f, g, _)) }
def sFun[A, B](s: B => LazyList[B]): Fun[A, B] => LazyList[Fun[A, B]] =
  { case Fun(ab, y) => sProduct(sPFun[A, B](s)(_), s)((ab, y))
    .map { case (ab, y) => Fun(ab, y) } }

trait CogenVariant[A] {
  def variant(r: Random, x: A): Random
  def cogen[B](f: A => B): A :=> B }

val unitCogenVariant: CogenVariant[Unit] =
  new CogenVariant[Unit] {
    def variant(r: Random, x: Unit): Random = r
    def cogen[B](f: Unit => B): Unit :=> B = Point(() => f(())) }
def eitherCogenVariant[A, B](ca: CogenVariant[A], cb: CogenVariant[B]):
  CogenVariant[Either[A, B]] = new CogenVariant[Either[A, B]] {
    def variant(r: Random, x: Either[A, B]): Random =
      x match { case Left(x) => ca.variant(r, x); case Right(x) => cb.variant(r, x) }
    def cogen[C](f: Either[A, B] => C): Either[A, B] :=> C =
      Choice(ca.cogen(x => f(Left(x))), cb.cogen(x => f(Right(x)))) }
def productCogenVariant[A, B](ca: CogenVariant[A], cb: CogenVariant[B]):
  CogenVariant[(A, B)] = new CogenVariant[(A, B)] {
    def variant(r: Random, x: (A, B)): Random = cb.variant(ca.variant(r, x._1), x._2)
    def cogen[C](f: ((A, B)) => C): (A, B) :=> C =
      Uncurry(ca.cogen(x => cb.cogen(y => f((x, y))))) }

def funGenShrink[A, B](c: CogenVariant[A], g: GenShrink[B]): GenShrink[Fun[A, B]] =
  GenShrink(functionGenQC(c.variant, g.g).map(c.cogen).product(g.g)
    .map { case (ab, y) => Fun(ab, y) }, sFun(g.s))

trait Cogen[A] { c =>
  def build[B](g: Gen[B]): Gen[A :=> B]
  def imap[Z](f: A => Z, h: Z => A): Cogen[Z] = new Cogen[Z] {
    def build[B](g: Gen[B]): Gen[Z :=> B] = c.build(g).map(Iso(h, f, _)) } }

val unitCogen: Cogen[Unit] = new Cogen[Unit] {
  def build[B](g: Gen[B]): Gen[Unit :=> B] =
    Gen { r => val (r1, r2) = r.split
      val f: Unit :=> Tree[B] = Point(() => g.gen(r2)._2)
      val t: Tree[Unit :=> Tree[B]] = Tree(f).expand(sPFun(_.ts))
      (r1, t.map(_.map(_.x)))} }
def eitherCogen[A, B](ca: Cogen[A], cb: Cogen[B]): Cogen[Either[A, B]] =
  new Cogen[Either[A, B]] { def build[C](g: Gen[C]): Gen[Either[A, B] :=> C] =
    ca.build(g).product(cb.build(g)).map { case (ga, gb) => Choice(ga, gb) } }
def productCogen[A, B](ca: Cogen[A], cb: Cogen[B]): Cogen[(A, B)] =
  new Cogen[(A, B)] { def build[C](g: Gen[C]): Gen[(A, B) :=> C] =
    ca.build(cb.build(g)).map(Uncurry(_)) }
val booleanCogen: Cogen[Boolean] =
  eitherCogen(unitCogen, unitCogen)
    .imap(_.isRight, x => if (x) Right(()) else Left(()))

def funGen[A, B](c: Cogen[A], g: Gen[B]): Gen[Fun[A, B]] =
  c.build(g).product(g).map { case (ab, y) => Fun(ab, y) }

case class LocalFun[A, B](x: A, y: B) extends (A => B) {
  def apply(x0: A): B = if (x == x0) y else ???
  override def toString: String = s"{case $x => $y}" }
def functionCogen[A, B](g: Gen[A], c: Cogen[B]): Cogen[A => B] =
  new Cogen[A => B] { def build[C](gc: Gen[C]): Gen[(A => B) :=> C] =
    g.product(c.build(gc)).map { case (x, bc) =>
      Iso(f => f(x), (y: B) => LocalFun(x, y), bc) } }

def listCogen[A](c: Cogen[A]): Cogen[List[A]] = new Cogen[List[A]] { cl0 =>
  val cl: Cogen[List[A]] = eitherCogen(unitCogen, productCogen(c, cl0))
    .imap(_.fold(_ => List.empty, { case (x, xs) => x :: xs}),
      { case Nil => Left(()); case x :: xs => Right((x, xs)) })
  def build[B](g: Gen[B]): Gen[List[A] :=> B] = Gen(cl.build(g).gen(_)) }

def conquerCogen: Cogen[Unit] = new Cogen[Unit] {
  def build[B](g: Gen[B]): Gen[Unit :=> B] = g.map(b => Point(() => b)) }
def emptyCogen[A]: Cogen[A] = new Cogen[A] {
  def build[B](g: Gen[B]): Gen[A :=> B] = Gen(r => (r, Tree(Empty()))) }

def genTrees[A](g: Gen[A]): LazyList[(Tree[A])] =
  LazyList.iterate((SplitMix64(42): Random, null: Tree[A])) {
    case (r, _) => g.gen(r)
  }.map(_._2).drop(1)
def findCounterExampleNoShrink[A](g: Gen[A])(p: A => Boolean): Option[A] =
  genTrees(g).take(100).find(t => !p(t.x)).map(_.x)
def findCounterExample[A](g: Gen[A])(p: A => Boolean): Option[A] =
  genTrees(g).take(100).find(t => !p(t.x))
    .map(t => shrinkQC(t, (t: Tree[A]) => t.ts, (t: Tree[A]) => p(t.x)).x)

val intGenQC = GenQC(r => r.next).map(_.toInt)
val intGen = Gen.from(GenShrink(intGenQC, sInt))
val booleanGen = intGen.map(_ % 2 == 0) }
