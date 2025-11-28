case class Libro(
                  titulo: String,
                  autor: String,
                  paginas: Int,
                  anio: Int)

case class AutorInfo(
                      autor: String,
                      totalPaginas: Int,
                      cantidadLibros: Int
                    )

val catalogo: List[Libro] = List(
  Libro("Programación en Scala", "Ana Ruiz", 150, 2010),
  Libro("Fundamentos de FP", "Juan Pérez", 165, 2011),
  Libro("Algoritmos Modernos", "Carlos León", 180, 2012),
  Libro("Estructuras de Datos", "María Gómez", 195, 2013),
  Libro("Introducción a la Programación", "Luis Andrade", 210, 2014),
  Libro("Técnicas de Depuración", "Ana Ruiz", 225, 2015),
  Libro("Diseño de Sistemas", "Juan Pérez", 240, 2016),
  Libro("Patrones de Diseño", "Carlos León", 255, 2017),
  Libro("Aplicaciones Web", "María Gómez", 270, 2018),
  Libro("Cómputo en la Nube", "Luis Andrade", 285, 2019),
  Libro("Arquitectura de Software", "Ana Ruiz", 300, 2015),
  Libro("Bases de Datos", "Juan Pérez", 315, 2016),
  Libro("Microservicios", "Carlos León", 330, 2017),
  Libro("Concurrencia en Java", "María Gómez", 345, 2018),
  Libro("Pruebas Automatizadas", "Luis Andrade", 360, 2019),
  Libro("Seguridad Aplicada", "Ana Ruiz", 375, 2016),
  Libro("DevOps Práctico", "Juan Pérez", 390, 2017),
  Libro("Análisis de Datos", "Carlos León", 405, 2018),
  Libro("Machine Learning Básico", "María Gómez", 420, 2019),
  Libro("Redes de Computadores", "Luis Andrade", 435, 2019)
)

def autoMasProductivo(
                    catalogo: List[Libro],
                    minPaginas: Int,
                    minAnio: Int
                     ): Option[AutorInfo] = {

// Tomar solo los libros que cumplen las condiciones (páginas y año).
val librosFiltrados: List [Libro] = catalogo.filter(l => l.paginas >= minPaginas && l.anio >= minAnio)

  if(librosFiltrados.isEmpty) return None

// A partir de esos libros filtrados, obtener la lista de autores sin repetir

def autoresUnicos(libros: List[Libro]): List[String] = {
  def aux(resto: List[Libro], vistos: List[String]): List[String] = resto match {
    case Nil => vistos
    case h :: t =>
      if (vistos.contains(h.autor)) aux(t, vistos)
      else aux(t, vistos :+ h.autor)



