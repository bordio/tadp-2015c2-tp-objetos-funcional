package ar.edu.tadp.dragonball

import ar.edu.tadp.dragonball.Movimientos._
import ar.edu.tadp.dragonball.seed._
import org.scalatest.{FunSpec, ShouldMatchers}

class GuerreroSpec extends FunSpec with ShouldMatchers {

  describe ("Constructor") {
    it ("Goku should have") {
      goku should have(
        'nombre ("goku"),
        'energia (100),
        'energiaMaxima (1000),
        'especie (Saiyajin(Normal,false)),
        'estado (Luchando),
        'movimientos (Ataques)
      )
    }
  }

  describe ("Movimientos") {
    describe ("CargarKi") {
      it ("Goku (Normal) al usar CargarKi, se le debe incrementar a 200 su energia") {
        goku.CargarEnergia(vegeta)._1 .energia should be (200)
      }
      it ("Trunks (SuperSaiyajin nivel 1) al usar CargarKi, se le debe incrementar a 1500 (1350 + 1*150) su energia") {
        trunks.CargarEnergia(goku) ._1 .energia should be (1500)
      }
      it ("Androide18 al usar CargarKi no debe tener ningun efecto por ser Androide") {
        androide18.CargarEnergia(trunks) ._1 .energia should be (900)
      }
    }

    describe ("DejarseFajar") {
      it ("Goku al DejarseFajar, el contador de rounds dejandose fajar debe estar en 1") {
        goku.DejarseFajarPor(vegeta) ._1 .estado should be (Fajado(1))
      }
      it ("Si Goku se deja fajar 2 veces consecutivas, el contador debe contar 2") {
        goku.DejarseFajarPor(vegeta)._1.DejarseFajarPor(vegeta)._1.estado should be (Fajado(2))
      }
//      it ("Si Goku se deja fajar 2 veces consecutivas, luego carga ki, y luego vuelve a dejarse fajar, el contador debe estar en 1") {
//        val estadoActual = DejarseFajar (CargarKi (DejarseFajar (DejarseFajar (goku, vegeta))))
//        estadoActual ._1 .estado should not be Fajado(3)
//        estadoActual ._1 .estado should be (Fajado(1))
//      }
    }

//    describe("Fusionarse"){
//      it("Goku se fusiona con Vegeta"){
//        val (fusionado:Guerrero,vegetaSigueIgual:Guerrero) = Fusionarse(goku)(vegeta,androide17)
//        fusionado.especie should be (Fusion)
//        fusionado.energia should be (vegeta.energia + goku.energia)
//        fusionado.energiaMaxima should be (vegeta.energiaMaxima + goku.energiaMaxima)
//        fusionado.movimientos should be (vegeta.movimientos ++ goku.movimientos)
//      }
//
//      it("Goku intenta fusionarse con Cell, no lo logra ya que cell no es fusionable"){
//        val (fusionado:Guerrero,cellSigueIgual:Guerrero) = Fusionarse(goku)(cell,androide17)
//        (fusionado,cellSigueIgual) should be (goku,cell)
//        fusionado.especie shouldNot be (Fusion)
//      }
//    }

//    describe ("Magia") {
//      it ("Piccolo hace pensar a Goku, y le deja su energia en 102") {
//        val (atacante, oponente) = Magia(hacertePensar)(piccolo,goku)
//        oponente.nombre should be(goku.nombre)
//        oponente.energia should be(102)
//      }
//      it ("Yamcha tiene las 7 esferas, por eso puede hacer magia y se queda sin esferas") {
//        val (atacante, oponente) = Magia(hacertePensar)(yamcha,goku)
//        oponente.nombre should be(goku.nombre)
//        oponente.energia should be(102)
//        atacante.tieneLas7Esferas should be (false)
//      }
//    }
  }
}
