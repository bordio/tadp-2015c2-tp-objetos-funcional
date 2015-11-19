package ar.edu.tadp

package object dragonball {



  /* ITEMS */
  trait Item

  case object ArmaFilosa extends Item

  case object ArmaRoma extends Item

  case object ArmaDeFuego extends Item

  case object SemillasDelErmitanio extends Item

  case class Municion(tipo: Item) extends Item

  case object FotoDeLaLuna extends Item

  case class Esfera(estrellas: Int) extends Item



  /* ESPECIES */

  trait Magico
  trait Fusionable

  abstract class Especie {
    def movimientosEspeciales: Set[Movimiento] = Set()
  }

  case object Humano extends Especie with Fusionable
  case object Androide extends Especie
  case object Namekusein extends Especie with Fusionable with Magico
  case object Fusionado extends Especie
  case class Saiyajin(estado: EstadoSaiyajin, tieneCola: Boolean = true) extends Especie with Fusionable

  /*Estados de la especie Saiyajin*/
  abstract class EstadoSaiyajin {
    def proximoNivelZ = 1
  }
  case object Normal extends EstadoSaiyajin
  case object MonoGigante extends EstadoSaiyajin
  case class SuperSaiyajin(nivel: Int) extends EstadoSaiyajin {
    override def proximoNivelZ = nivel + 1
  }

  case class Monstruo(tipoDigestion: TipoDigestion, guerrerosComidos: List[Guerrero]) extends Especie {
    override def movimientosEspeciales = {
      tipoDigestion(guerrerosComidos)
    }
  }

  /*Tipos de digestion de la especie Monstruo*/

  type TipoDigestion = List[Guerrero] => Set[Movimiento]

  val digestionMajinBoo = (guerreros: List[Guerrero]) => {
    guerreros.last.movimientosPropios
  }

  /* ESTADOS */
  abstract class Estado
  case object Luchando extends Estado
  case class Fajado(rounds: Int) extends Estado
  case object KO extends Estado
  case object Muerto extends Estado
  case object Reposo extends Estado


  /* PLAN DE ATAQUE */

  case class PlanDeAtaque(movimientos: List[Movimiento] = List()) {
    def agregarMovimiento(movimiento: Movimiento) =
      copy(movimientos = movimientos :+ movimiento)
  }



  /* RESULTADOS DE PELEA */

  trait ResultadoPelea

  case class Ganador(ganador: Guerrero) extends ResultadoPelea

  case class SiguenPeleando(atacante: Guerrero, oponente: Guerrero) extends ResultadoPelea



  /* MOVIMIENTOS */

  type Movimiento = Guerrero => Guerrero => (Guerrero, Guerrero)

  val dejarseFajar: Movimiento = (atacante: Guerrero) => (oponente: Guerrero) => (atacante.aumentarRoundsDejandoseFajar(), oponente)

  val cargarKi: Movimiento = (atacante: Guerrero) => (oponente: Guerrero) => {
    atacante.especie match {
      case (Saiyajin(SuperSaiyajin(nivel), _)) =>
        (atacante.aumentarEnergia(150 * nivel), oponente)
      case Androide =>
        (atacante, oponente)
      case _ =>
        (atacante.aumentarEnergia(100), oponente)
    }
  }

  case class UsarItem(item: Item) extends Movimiento {
    def apply(atacante: Guerrero) = (oponente: Guerrero) => {

      if (atacante.tieneItem(item)) {
        (item, oponente.especie, oponente.estado) match {
          case (ArmaRoma, Androide, _) => (atacante, oponente)
          case (ArmaRoma, _, _) => (atacante, oponente.quedarKOSiEnergiaMenorA(300))

          case (ArmaFilosa, Saiyajin(MonoGigante, _),_) =>
            (atacante, oponente.cambiarEspecieA(Saiyajin(Normal, tieneCola = false)).cambiarEnergiaA(1).cambiarEstadoA(KO))
          case (ArmaFilosa, Saiyajin(estado, tieneCola), _) if tieneCola =>
            (atacante, oponente.cambiarEspecieA(Saiyajin(estado, tieneCola = false)).cambiarEnergiaA(1))
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

  val comerseAlOponente: Movimiento = (atacante: Guerrero) => (oponente: Guerrero) => {
    atacante.especie match {
      case Monstruo(tipoDigestion, guerrerosComidos) if oponente.energia < atacante.energia =>
        (atacante.comerseA(oponente, tipoDigestion, guerrerosComidos), oponente.cambiarEstadoA(Muerto))
      case _ =>
        //atacante.pasarVerguenza()
        (atacante, oponente)
    }
  }

  val convertirseEnMono: Movimiento = (atacante: Guerrero) => (oponente: Guerrero) => {
    atacante.especie match {
      case Saiyajin(_, tieneCola) if tieneCola && atacante.tieneFotoDeLuna() =>
        (atacante.cambiarEstadoSaiyajin(MonoGigante,tieneCola).recuperarEnergiaMaxima().multiplicarEnergiaMaximaPor(3), oponente)
      case _ =>
        (atacante, oponente)
    }
  }

  val convertirseEnSuperSaiyajin: Movimiento = (atacante: Guerrero) => (oponente: Guerrero) => {
    atacante.especie match {
      case Saiyajin(estado, tieneCola) if atacante.puedeSubirDeNivel() =>
        (atacante.cambiarEstadoSaiyajin(SuperSaiyajin(0),tieneCola).multiplicarEnergiaMaximaPor(5), oponente)
      case _ =>
        (atacante, oponente)
    }
  }


  case class Magia(estado: Estado, objetivo: Guerrero) extends Movimiento {
    def apply(atacante: Guerrero) = (oponente: Guerrero) => {
      atacante.especie match {
        case Namekusein | Monstruo(_, _) | _ if atacante.tieneLas7Esferas() =>
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

  val muchosGolpesNinja: Movimiento = (atacante: Guerrero) => (oponente: Guerrero) => {
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

  val explotar: Movimiento = (atacante: Guerrero) => (oponente: Guerrero) => {
    (atacante.especie, oponente.especie) match {
      case (Monstruo(_, _), Namekusein) =>
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
      case (Monstruo(_, _), _) =>
        (atacante.cambiarEnergiaA(0).cambiarEstadoA(Muerto), oponente.reducirEnergia(2 * atacante.energia))
      case (Androide, _) =>
        (atacante.cambiarEnergiaA(0).cambiarEstadoA(Muerto), oponente.reducirEnergia(3 * atacante.energia))
      case (_) =>
        (atacante, oponente)
    }
  }

  case class onda(energiaRequerida: Int) extends Movimiento {
    def apply(atacante: Guerrero) = (oponente: Guerrero) => {
      oponente.especie match {
        case Monstruo(_, _) if atacante.energia > energiaRequerida =>
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

  val genkidama: Movimiento = (atacante: Guerrero) => (oponente: Guerrero) => {
    oponente.especie match {
      case Androide =>
        (atacante, oponente.aumentarEnergia(10 ^ atacante.roundsDejandoseFajar))
      case _ =>
        (atacante, oponente.reducirEnergia(10 ^ atacante.roundsDejandoseFajar))
    }
  }

  //CRITERIO
  type Criterio = (Guerrero, Guerrero) => Int

  val criterioEnergia = (atacante: Guerrero, oponente: Guerrero) => {
    atacante.energia
  }
}
