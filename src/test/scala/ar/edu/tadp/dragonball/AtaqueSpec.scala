package ar.edu.tadp.dragonball

import ar.edu.tadp.dragonball.Movimientos._
import ar.edu.tadp.dragonball.seed._
import org.scalatest._

class AtaqueSpec extends FunSpec with ShouldMatchers {
  describe("Ataques fisico") {
    describe("MuchosGolpesNinjas") {
      it("Yajirobe le pega a androide18 pero se hace daño a si mismo") {
        yajirobe.MuchosGolpesNinjasA(androide18)._1.energia should be(390)
      }
      it("Yajirobe le pega a androide16 que es más débil que él pero se hace daño a si mismo") {
        yajirobe.MuchosGolpesNinjasA(androideDebil)._1.energia should be(390)
      }
      it("Yajirobe quiere pegarle a vegeta pero no logra más que hacerse daño a si mismo") {
        yajirobe.MuchosGolpesNinjasA(vegeta)._1.energia should be(380)
      }
    }

    describe ("Explotar") {
      it ("El androide16 es muy debil, y apesar de que explota no logra matar al oponente") {
        val (androideMuerto, piccoloRePiyo) = androideDebil InmolarseContra piccolo
        androideMuerto.estado should be(Muerto)
        piccoloRePiyo.energia should be(600)
      }
      it ("El androide18 explota, y mata a yajirobe") {
        val (androideMuerto, yajirobeMuerto) = androide18 InmolarseContra yajirobe
        androideMuerto.estado should be(Muerto)
        yajirobeMuerto.estado should be(Muerto)
      }
      it ("El androide18 explota, y pero no puede matar al gran Piccolo Daimaku porque es namekuseiano") {
        val (androideMuerto, piccoloExplotado) =  androide18 InmolarseContra piccolo
        androideMuerto.estado should be(Muerto)
        piccoloExplotado.estado should be(Luchando)
        piccoloExplotado.energia should be(1)
      }
    }
  }

  describe("Ataques con energia") {
    describe("Onda") {
      it("Vegeta le tira un kame hame a yajirobe!") {
        vegeta.KameHame(150)(yajirobe)._1.energia should be(350)
        vegeta.KameHame(150)(yajirobe)._2.energia should be(100)
      }
      it("Goku trata de tirar un kame hame de 200 pero no tiene energia suficiente") {
        goku.KameHame(200)(vegeta)._1.energia should be(100)
      }
      it("Trunks ingenuamente trata de atacar con Kame Hame a androide18, pero lo cura") {
        trunks.KameHame(200)(androide18)._2.energia should be(1300)
        trunks.KameHame(200)(androide18)._1.energia should be(1150)
      }
    }

    describe ("Genkidama") {
      it ("Goku se deja fajar 3 turnos y saca 1000") {
        val gokuDejandoseFajar = goku estas Fajado(3)
        gokuDejandoseFajar.LanzarGenkidamaContra(trunks) ._2 .energia should be(350)
      }
      it ("Goku asesina a vegeta con una genkidama en 3 turnos") {
        val gokuFajado = goku estas Fajado(3)
        gokuFajado.LanzarGenkidamaContra(vegeta) ._2 .estado should be(Muerto)
      }
      it ("Goku se ceba queriendo matar a todos con su genkidama, pero al androide lo cura") {
        val gokuFajado = goku estas Fajado(3)
        gokuFajado.LanzarGenkidamaContra(androideDebil) ._2 .energia should be(300)
      }
    }
  }
}
