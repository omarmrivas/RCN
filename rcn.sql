-- phpMyAdmin SQL Dump
-- version 4.5.5.1
-- http://www.phpmyadmin.net
--
-- Servidor: localhost
-- Tiempo de generación: 22-03-2016 a las 23:04:13
-- Versión del servidor: 5.6.25
-- Versión de PHP: 5.5.31

SET SQL_MODE = "NO_AUTO_VALUE_ON_ZERO";
SET time_zone = "+00:00";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8mb4 */;

--
-- Base de datos: `rcn`
--

DELIMITER $$
--
-- Funciones
--
CREATE DEFINER=`rcn`@`%` FUNCTION `MAXIMUM` () RETURNS BIGINT BEGIN
  DECLARE retVal BIGINT;
  SELECT MAX(id) into retval
  FROM Stack;
  RETURN retval;
END$$

DELIMITER ;

-- --------------------------------------------------------

--
-- Estructura de tabla para la tabla `Stack`
--

CREATE TABLE `Stack` (
  `id` BIGINT NOT NULL,
  `depth` int(11) NOT NULL,
  `vertices` int(11) NOT NULL,
  `graph` longblob NOT NULL,
  `solution` mediumblob NOT NULL,
  `triangles` int(11) NOT NULL,
  `polygons` int(11) NOT NULL,
  `crossings` int(11) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin;

--
-- Estructura de tabla para la tabla `Stack`
--

CREATE TABLE `Graphs` (
  `id` int(11) NOT NULL,
  `vertices` int(11) NOT NULL,
  `graph` longblob NOT NULL,
  `solution` mediumblob NOT NULL,
  `triangles` int(11) NOT NULL,
  `polygons` int(11) NOT NULL,
  `crossings` int(11) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin;


--
-- Índices para tablas volcadas
--

--
-- Indices de la tabla `Stack`
--
ALTER TABLE `Stack`
  ADD PRIMARY KEY (`id`);

--
-- Indices de la tabla `Graphs`
--
ALTER TABLE `Graphs`
  ADD PRIMARY KEY (`id`);

--
-- AUTO_INCREMENT de las tablas volcadas
--

--
-- AUTO_INCREMENT de la tabla `Stack`
--
ALTER TABLE `Stack`
  MODIFY `id` BIGINT NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=1;
  
--
-- AUTO_INCREMENT de la tabla `Graphs`
--
ALTER TABLE `Graphs`
  MODIFY `id` int(11) NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=1;
  
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
