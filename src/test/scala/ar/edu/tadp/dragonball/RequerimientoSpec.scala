package ar.edu.tadp.dragonball

import ar.edu.tadp.dragonball.Criterios._
import ar.edu.tadp.dragonball.Movimientos._
import ar.edu.tadp.dragonball.seed._
import org.scalatest._

class RequerimientoSpec extends FunSpec with ShouldMatchers {
  describe ("movimientoMasEfectivoContra") {
    it("Goku elige CargarKi porque lo deja con mas energia") {
      goku.movimientoMasEfectivoContra(vegeta)(quedarConMasEnergia).get(goku)(_:Guerrero) should be(CargarKi(goku)(_:Guerrero))
    }
    it ("Vegeta elige Onda(150) para quedar con menos energia") {
      vegeta.movimientoMasEfectivoContra(goku)(quedarConMenosEnergia).get(goku)(_:Guerrero) should be(Onda(150)(goku)(_:Guerrero))
    }
    it ("Si Goku pelea contra un Androide y quiere quedar con menos energia, entonces debe elegir MuchosGolpesNinjas") {
      goku.movimientoMasEfectivoContra(androide18)(quedarConMenosEnergia).get(goku)(_:Guerrero) should be(MuchosGolpesNinja(goku)(_:Guerrero))
    }
    it ("Androide18 elige Onda(150) para quedar con mas energia") {
      androide18.movimientoMasEfectivoContra(yajirobe)(quedarConMasEnergia).get(goku)(_:Guerrero) should be(Onda(150)(goku)(_:Guerrero))
    }
  }

  describe ("pelearUnRound") {
    it ("Goku se deja fajar en round, pero vegeta no aprovecha la oportunidad") {
      val (gokuDespues, vegetaDespues) = goku.pelearUnRound(DejarseFajar)(vegeta)
      gokuDespues.estado should be(Fajado(1))
    }
    it ("El androide18 saca a pasear a Yajirobe") {
      val (yajirobeDespues, androideDespues) = yajirobe.pelearUnRound(MuchosGolpesNinja)(androide18)
      yajirobeDespues.energia should be(90) //se hizo 10 a si mismo, y el androide lo ataco con Onda(150)
    }
  }

//  describe ("planDeAtaqueContra") {
//    it ("Goku para quedar con mas energia durante dos turnos, siempre elige CargarKi") {
//      goku.planDeAtaqueContra(vegeta, 2)(quedarConMasEnergia) should be(List[Option[Movimiento]](Some(CargarKi), Some(Onda(150))))
//    }
//    it ("Cell es demasiado groso para piccolo, y de guapo no mas, se deja Fajar 3 turnos seguidos.") {
//      cell.planDeAtaqueContra(piccolo, 3)(quedarConMenosEnergia) should be(List[Movimiento](DejarseFajar, DejarseFajar, DejarseFajar))
//    }
//    it ("Yajirobe ataca a Goku") {
//      yajirobe.planDeAtaqueContra(cell, 2)(quedarConMasEnergia) should be(List[Movimiento](UsarItem(ArmaFilosa), UsarItem(SemillaDelErmitanio)))
//    }
//  }

  describe ("pelearContra") {
    it ("Yamcha no le gana ni a Yajirobe") {
      yamcha.pelearContra(yajirobe)(yamcha.planDeAtaqueContra(yajirobe, 3)(quedarConMenosEnergia)) should be(Ganador(yajirobe.cambiarEnergiaA(400)))
    }
    it ("Goku y Vegeta quedan peleando luego de 2 rounds") {
      goku.pelearContra(vegeta)(goku.planDeAtaqueContra(vegeta, 2)(quedarConMasEnergia)) should be(SiguenPeleando(goku.cambiarEnergiaA(60),vegeta.cambiarEnergiaA(700)))
    }
    it ("A pesar de que vegeta empieza atacando, majin boo lo extermina en 16 turnos") {
      vegeta.recuperarEnergiaMaxima.pelearContra(majinBoo)(vegeta.planDeAtaqueContra(majinBoo, 16)(quedarConMasEnergia)) should be(Ganador(majinBoo.cambiarEnergiaA(2775)))
    }
  }
}
