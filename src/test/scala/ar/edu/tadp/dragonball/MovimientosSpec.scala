package ar.edu.tadp.dragonball

import org.scalatest.{FlatSpec, Matchers}

class MovimientosSpec extends FlatSpec with Matchers {

  val goku: Guerrero = Guerrero("Goku", List(SemillasDelErmitanio), 9500, 20000, List(dejarseFajar, cargarKi, UsarItem(SemillasDelErmitanio)), Saiyajin(Normal), Reposo)
  val vegeta: Guerrero = Guerrero("Vegeta", List(FotoDeLaLuna), 8000, 9000, List(dejarseFajar, convertirseEnMono, convertirseEnSuperSaiyajin), Saiyajin(Normal), Reposo)
  val gohan: Guerrero = Guerrero("Gohan", List(FotoDeLaLuna), 7900, 9000, List(dejarseFajar, cargarKi), Saiyajin(Normal),Reposo)

  "Goku" should "dejarse fajar por Vegeta sin modificar nada" in {
    val (atacante: Guerrero, defensor: Guerrero) = goku.realizarMovimiento(vegeta, dejarseFajar)

    atacante.energia should be (9500) //IT'S OVER 9000!!!
    defensor.energia should be (8000)
  }

  "Goku" should "aumentar su energia cuando usa cargar ki" in {
    val (atacante: Guerrero, defensor: Guerrero) = goku.realizarMovimiento(vegeta, cargarKi)

    atacante.energia should be (9600)
    defensor.energia should be (8000)
  }

  "Goku" should "recuperar toda su energia cuando come una semilla del ermitanio" in {
    val (atacante: Guerrero, defensor: Guerrero) = goku.realizarMovimiento(vegeta, UsarItem(SemillasDelErmitanio))

    atacante.energia should be (20000)
    defensor.energia should be (8000)
  }

  "Vegeta" should "aumentar su energia a 9000 y su energia maxima a 27000 al convertirse en mono" in {
    val (atacante: Guerrero, defensor: Guerrero) = vegeta.realizarMovimiento(goku, convertirseEnMono)

    atacante.especie should be (Saiyajin(MonoGigante,true))
    atacante.energia should be (9000)
    atacante.energiaMaxima should be (27000)
  }

  "Vegeta" should "al convertirse en super Saiyajin" in {
    val (atacante: Guerrero, defensor: Guerrero) = vegeta.realizarMovimiento(goku, convertirseEnSuperSaiyajin)

    atacante.especie should be (Saiyajin(SuperSaiyajin(0),true))
    atacante.energia should be (8000)
    atacante.energiaMaxima should be (45000)
  }

  "CargarKi" should "ser mas efectivo que dejarse fajar con el criterio de energia del atacante" in{
    val criterio = new CriterioEnergia()

    val movimiento = gohan.movimientoMasEfectivoContra(vegeta)(criterio)

    movimiento.hashCode() should be (cargarKi.hashCode())

  }

}
