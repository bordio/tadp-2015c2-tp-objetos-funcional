package ar.edu.tadp.dragonball

import ar.edu.tadp.dragonball.Criterios._
import ar.edu.tadp.dragonball.Movimientos._
import ar.edu.tadp.dragonball.seed._
import org.scalatest._

class RequerimientoSpec extends FunSpec with ShouldMatchers {
//  describe ("movimientoMasEfectivoContra") {
//    it ("Goku elige CargarKi porque lo deja con mas energia") {
//      goku.movimientoMasEfectivoContra(vegeta)(quedarConMasEnergia).get should be(CargarKi)
//    }
//    it ("Vegeta elige Onda(150) para quedar con menos energia") {
//      vegeta.movimientoMasEfectivoContra(goku)(quedarConMenosEnergia).get should be(Onda(150))
//    }
//    it ("Si Goku pelea contra un Androide y quiere quedar con menos energia, entonces debe elegir MuchosGolpesNinjas") {
//      goku.movimientoMasEfectivoContra(androide18)(quedarConMenosEnergia).get should be(MuchosGolpesNinja)
//    }
//    it ("Androide18 elige Onda(150) para quedar con mas energia") {
//      androide18.movimientoMasEfectivoContra(yajirobe)(quedarConMasEnergia).get should be(Onda(150))
//    }
//  }
//
//  describe ("pelearUnRound") {
//    it ("Goku se deja fajar en round, pero vegeta no aprovecha la oportunidad") {
//      val (gokuDespues, vegetaDespues) = goku.pelearUnRound(DejarseFajar)(vegeta)
//      gokuDespues.estado should be(Fajado(1))
//    }
//    it ("El androide18 saca a pasear a Yajirobe") {
//      val (yajirobeDespues, androideDespues) = yajirobe.pelearUnRound(MuchosGolpesNinja)(androide18)
//      yajirobeDespues.energia should be(90) //se hizo 10 a si mismo, y el androide lo ataco con Onda(150)
//    }
//  }
//
//  describe ("planDeAtaqueContra") {
//    it ("Goku para quedar con mas energia durante dos turnos, siempre elige CargarKi") {
//      goku.planDeAtaqueContra(vegeta, 2)(quedarConMasEnergia).get should be(List(CargarKi, Onda(150)))
//    }
//    it ("Cell es demasiado groso para piccolo, y de guapo no mas, se deja Fajar 3 turnos seguidos.") {
//      cell.planDeAtaqueContra(piccolo, 3)(quedarConMenosEnergia).get should be(List(DejarseFajar, DejarseFajar, DejarseFajar))
//    }
//    it ("Yajirobe ataca a Goku") {
//      yajirobe.planDeAtaqueContra(cell, 2)(quedarConMasEnergia).get should be(List(UsarItem(ArmaFilosa), UsarItem(SemillaDelErmitanio)))
//    }
//  }
//
//  describe ("pelearContra") {
//    it ("Yajirobe le gana a Goku") {
//      yajirobe.pelearContra(goku)(yajirobe.planDeAtaqueContra(goku, 3)(quedarConMasEnergia).get) should be(Ganador(yajirobe.cambiarEnergiaA(100).estas(Luchando)))
//    }
//    it ("Goku y Vegeta quedan peleando luego de 2 rounds") {
//      goku.pelearContra(vegeta)(goku.planDeAtaqueContra(vegeta, 2)(quedarConMasEnergia).get) should be(SiguenPeleando(goku.cambiarEnergiaA(50),vegeta.cambiarEnergiaA(400)))
//    }
//    it ("MajinBoo tarda 42 turnos en matar a Vegeta cuando se transforma en Mono") {
//      val (vegetaMono,majinboo) = ConvertirseEnMono (vegeta, majinBoo)
//      vegetaMono.recuperarEnergiaMaxima.pelearContra(majinboo)(vegetaMono.planDeAtaqueContra(majinboo, 42)(quedarConMasEnergia).get) should be(Ganador(majinBoo.cambiarEnergiaA(2950)))
//    }
//  }
}
