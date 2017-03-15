package com.datastax.spark.connector.util

import org.apache.spark.SparkConf

case class ConfigParameter[T](
  val name: String,
  val section: String,
  val default: T,
  val description: String) extends DataFrameOption {

  override val sqlOptionName = name.replaceAll("\\.", "\\_")

  def option(value: Any): Map[String, String] = {
    require(value != null)
    Map(name -> value.toString)
  }

}

/**
  * Class representing a Config Parameter no longer in use and it's replacement if any. Use
  * rational to display more information about the deprecation and what to do for the end user.
  */
case class DeprecatedConfigParameter (
  val oldName: String,
  val newName: Option[String],
  val rational: String = "") extends Logging {

  def explanation: String = newName match {
    case Some(replacement) => s"$oldName is deprecated and has been replaced with parameter ${newName.get}. $rational"
    case None => s"$oldName is deprecated and there is no replacement. $rational"
  }

  def maybeReplace(sparkConf: SparkConf): Unit = {
    sparkConf.getOption(oldName) match {
      case Some(value) => {
        newName match {
          case Some(name) =>
            sparkConf.remove(oldName)
            sparkConf.set(name, value)
            logWarning(explanation)
          case None =>
            throw new IllegalArgumentException(explanation)
        }
      }
      case None =>
    }
  }
}
