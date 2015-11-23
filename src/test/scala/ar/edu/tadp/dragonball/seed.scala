package ar.edu.tadp.dragonball

import ar.edu.tadp.dragonball.Movimientos._
import ar.edu.tadp.dragonball.TiposDeDigestion._

package object seed {
  val Ataques: List[Movimiento] = List(DejarseFajar, CargarKi, MuchosGolpesNinjas, Onda(150), Genkidama)
  def tieneMasEnergia(guerreros: Guerreros) = guerreros._1.energia
  //val AtaquesDeYajirobe: List[Movimiento] = List(MuchosGolpesNinja, UsarItem(ArmaFilosa), UsarItem(SemillaDelErmitanio))
  val hacertePensar: Function1[Guerreros,Guerreros] = { case (atacante,oponente) => (atacante, oponente.actualizarEnergia(2)) }
  val goku: Guerrero = Guerrero("goku", List(FotoDeLaLuna, EsferaDelDragon(4)), 100, 1000, Saiyajin(Normal,false), Luchando, Ataques)
  val vegeta: Guerrero = Guerrero("vegeta", List(FotoDeLaLuna,SemillaDelErmitanio), 500, 1000, Saiyajin(Normal), Luchando, Ataques/* ++ List(UsarItem(FotoDeLaLuna))*/)
  val trunks: Guerrero = Guerrero("trunks", List(SemillaDelErmitanio, ArmaFilosa), 1350, 2000, Saiyajin(SuperSaiyajin(1), cola = false), Luchando, Ataques)
  val androide18: Guerrero = Guerrero("Androide18", List(ArmaDeFuego, Municion(ArmaDeFuego)), 900, 1800, Androide, Luchando, Ataques ++ List[Movimiento](Explotar)/*,UsarItem(FotoDeLaLuna))*/)
  val androide17: Guerrero = Guerrero("Androide17", List(ArmaDeFuego, Municion(ArmaDeFuego)), 1800, 5000, Androide, Luchando, Ataques ++ List[Movimiento](Explotar)/*,UsarItem(FotoDeLaLuna))*/)
  val yajirobe: Guerrero = Guerrero("Yajirobe", List(SemillaDelErmitanio), 400, 400, Humano, Luchando, Ataques)
  val androideDebil: Guerrero = Guerrero("Androide16", List(ArmaDeFuego, Municion(ArmaDeFuego)), 200, 300, Androide, Luchando, Ataques ++ List[Movimiento](Explotar)/*, Onda(80))*/)
  val cell: Guerrero = Guerrero("Cell", List(EsferaDelDragon(3)), 1200, 3000, Monstruo(digestionCell,List()), Luchando, Ataques ++ List[Movimiento](ComerseAlOponente))
  val majinBoo: Guerrero = Guerrero("MajinBoo", List(), 2000, 3000, Monstruo(digestionMajinBoo, List()), Luchando, Ataques ++ List[Movimiento](ComerseAlOponente))
  val piccolo: Guerrero = Guerrero("Piccolo", List(), 1000, 1000, Namekusein, Luchando, Ataques/* ++ List(Magia(hacertePensar))*/)
  val yamcha: Guerrero = Guerrero("Yamcha",List(EsferaDelDragon(1),EsferaDelDragon(2),EsferaDelDragon(3),EsferaDelDragon(4),EsferaDelDragon(5),EsferaDelDragon(6),EsferaDelDragon(7)),50,200,Humano,Luchando,Ataques)
}
