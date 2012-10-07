<?xml version="1.0" encoding="ISO-8859-1"?>
<!-- $Id: getRSS2el.xsl,v 1.9 2003/11/01 15:42:24 mgarrido Exp $ -->       

<!-- Transformación para una sola fuente -->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
     xmlns:dc="http://purl.org/dc/elements/1.1/" version="1.0">

  <xsl:output method="text"/>

  <!-- Esperamos un parámetro con la función para "help-insert-xref-button" -->
  <xsl:param name="func" />

  <!-- Se procesa cada fuente de información -->
  <xsl:template match="/">
    <xsl:apply-templates select="//*[local-name()='item']"/>
  </xsl:template>

  <!-- Procesamos las noticias -->
  <xsl:template match="*[local-name()='item']">
      <xsl:text>(insert "\t")(help-insert-xref-button "</xsl:text>
      <xsl:apply-templates select="*[local-name()='title']"/>
      <xsl:text>" '</xsl:text>
      <xsl:value-of select="$func" />
      <xsl:text> "</xsl:text>
      <xsl:value-of select="*[local-name()='link']"/>
      <xsl:text>")(insert "\n")</xsl:text>
  </xsl:template>

  <xsl:template name="filter-input">
      <!-- Sustituyo las comillas dobles por culebrillas -->
      <xsl:value-of select="translate(., '&#34;', '&#126;')"/>
  </xsl:template>

  <xsl:template match="*[local-name()='title']">
      <xsl:call-template name="filter-input"/>
  </xsl:template>

</xsl:stylesheet>