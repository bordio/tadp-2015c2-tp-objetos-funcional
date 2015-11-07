package ar.edu.tadp.dragonball

import ar.edu.tadp.dragonball.DragonBall._


object Movimientos {

  type Movimiento = (Guerrero, Guerrero) => (Guerrero, Guerrero)



  val dejarseFajar = (atacante: Guerrero, oponente: Guerrero) => (atacante.aumentarRoundsDejandoseFajar(), oponente)

  val cargarKi = (atacante: Guerrero, oponente: Guerrero) => {
    (atacante.especie, atacante.estado) match {
      case (Saiyajin(nivel, _), SuperSaiyajin) =>
        (atacante.aumentarEnergia(150 * nivel), oponente)
      case (Androide, _) =>
        (atacante, oponente)
      case (_) =>
        (atacante.aumentarEnergia(100), oponente)
    }
  }

  case class UsarItem(item: Item) extends Movimiento {
    def apply(atacante: Guerrero, oponente: Guerrero) = {

      if (atacante.tieneItem(item)) {
        (item, oponente.especie, oponente.estado) match {
          case (ArmaRoma, Androide, _) => (atacante, oponente)
          case (ArmaRoma, _, _) => (atacante, oponente.quedarKOSiEnergiaMenorA(300))

          case (ArmaFilosa, Saiyajin(nivel,_), MonoGigante) =>
            (atacante, oponente.cambiarEspecieA(Saiyajin(nivel, tieneCola = false)).cambiarEnergiaA(1).cambiarEstadoA(KO))
          case (ArmaFilosa, Saiyajin(nivel, tieneCola), _) if tieneCola =>
            (atacante, oponente.cambiarEspecieA(Saiyajin(nivel, tieneCola = false)).cambiarEnergiaA(1))
          case (ArmaFilosa, _, _) =>
            (atacante, oponente.reducirEnergia(atacante.energia / 100))

          //TODO: Restar una municion del inventario
          case (ArmaDeFuego, Humano, _) =>
            (atacante, oponente.reducirEnergia(20))
          case (ArmaDeFuego, Namekusein, KO) =>
            (atacante, oponente.reducirEnergia(10))

          case (SemillasDelErmitanio, _, _) =>
            (atacante.recuperarEnergiaMaxima().eliminarItem(item), oponente)
        }
      } else {
        (atacante, oponente)
      }
    }
  }

  val comerseAlOponente = (atacante: Guerrero, oponente: Guerrero) => {
    atacante.especie match {
      case Monstruo(tipoDigestion) if oponente.energia < atacante.energia =>
        (atacante.comerseA(oponente, tipoDigestion), oponente.cambiarEstadoA(Muerto))
      case _ =>
        //atacante.pasarVerguenza()
        (atacante, oponente)
    }
  }

  val convertirseEnMono = (atacante: Guerrero, oponente: Guerrero) => {
    atacante.especie match {
      case Saiyajin(_, tieneCola) if tieneCola && atacante.tieneFotoDeLuna() =>
        (atacante.cambiarEstadoA(MonoGigante).recuperarEnergiaMaxima().multiplicarEnergiaMaximaPor(3), oponente)
      case _ =>
        (atacante, oponente)
    }
  }

  val convertirseEnSuperSaiyajin = (atacante: Guerrero, oponente: Guerrero) => {
    atacante.especie match {
      case Saiyajin(nivel, tieneCola) if atacante.puedeSubirDeNivel() =>
        (atacante.cambiarEstadoA(SuperSaiyajin).multiplicarEnergiaMaximaPor(5).cambiarEspecieA(Saiyajin(nivel + 1, tieneCola)), oponente)
      case _ =>
        (atacante, oponente)
    }
  }

  case class Fusion(amigo: Guerrero) extends Movimiento {
    def apply(atacante: Guerrero, oponente: Guerrero) = {
      (atacante.especie, amigo.especie) match {
        case (Humano, Humano) |
             (Humano, Saiyajin(_,_)) |
             (Humano, Namekusein) |
             (Saiyajin(_,_), Humano) |
             (Saiyajin(_,_), Saiyajin(_,_)) |
             (Saiyajin(_,_), Namekusein) |
             (Namekusein, Humano) |
             (Namekusein, Saiyajin(_,_)) |
             (Namekusein, Namekusein) =>
          (atacante.aumentarEnergia(amigo.energia).aumentarEnergiaMaxima(amigo.energiaMaxima).cambiarEspecieA(Indefinido), oponente)
        case (_) =>
          (atacante, oponente)
      }
    }
  }

  case class Magia(estado: Estado, objetivo: Guerrero) extends Movimiento {
    def apply(atacante: Guerrero, oponente: Guerrero) = {
      atacante.especie match {
        case Namekusein | Monstruo(_) | _ if atacante.tieneLas7Esferas() =>
          if (objetivo == atacante) {
            (objetivo.cambiarEstadoA(estado), oponente)
          } else if (objetivo == oponente) {
            (atacante, oponente.cambiarEstadoA(estado))
          } else {
            (atacante, oponente)
          }
        case (_) =>
          (atacante, oponente)
      }
    }
  }

  val muchosGolpesNinja = (atacante: Guerrero, oponente: Guerrero) => {
    (atacante.especie, oponente.especie) match {
      case (Humano, Androide) =>
        (atacante.reducirEnergia(10), oponente)
      case (_) =>
        if (atacante.energia >= oponente.energia) {
          (atacante, oponente.reducirEnergia(20))
        } else {
          (atacante.reducirEnergia(20), oponente)
        }
    }
  }

  val explotar = (atacante: Guerrero, oponente: Guerrero) => {
    (atacante.especie, oponente.especie) match {
      case (Monstruo(_), Namekusein) =>
        var valorAReducir = 2 * atacante.energia
        if (valorAReducir >= oponente.energia) {
          valorAReducir = oponente.energia - 1
        }
        (atacante.cambiarEnergiaA(0).cambiarEstadoA(Muerto), oponente.reducirEnergia(valorAReducir))
      case (Androide, Namekusein) =>
        var valorAReducir = 3 * atacante.energia
        if (valorAReducir >= oponente.energia) {
          valorAReducir = oponente.energia - 1
        }
        (atacante.cambiarEnergiaA(0).cambiarEstadoA(Muerto), oponente.reducirEnergia(valorAReducir))
      case (Monstruo(_), _) =>
        (atacante.cambiarEnergiaA(0).cambiarEstadoA(Muerto), oponente.reducirEnergia(2 * atacante.energia))
      case (Androide, _) =>
        (atacante.cambiarEnergiaA(0).cambiarEstadoA(Muerto), oponente.reducirEnergia(3 * atacante.energia))
      case (_) =>
        (atacante, oponente)
    }
  }

  case class onda(energiaRequerida: Int) extends Movimiento {
    def apply(atacante: Guerrero, oponente: Guerrero) = {
      oponente.especie match {
        case Monstruo(_) if atacante.energia > energiaRequerida =>
          (atacante.reducirEnergia(energiaRequerida), oponente.reducirEnergia(energiaRequerida / 2))
        case Androide if atacante.energia > energiaRequerida =>
          (atacante.reducirEnergia(energiaRequerida), oponente.aumentarEnergia(energiaRequerida))
        case _ if atacante.energia > energiaRequerida =>
          (atacante.reducirEnergia(energiaRequerida), oponente.reducirEnergia(energiaRequerida * 2))
        case _ =>
          (atacante, oponente)
      }
    }
  }

  val genkidama = (atacante: Guerrero, oponente: Guerrero) => {
    oponente.especie match {
      case Androide =>
        (atacante, oponente.aumentarEnergia(10 ^ atacante.roundsDejandoseFajar))
      case _ =>
        (atacante, oponente.reducirEnergia(10 ^ atacante.roundsDejandoseFajar))
    }
  }

}
