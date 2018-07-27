<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:fo="http://www.w3.org/1999/XSL/Format" xmlns:xs="http://www.w3.org/2001/XMLSchema"
                xmlns:fn="http://www.w3.org/2005/xpath-functions"
                xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/"
                xmlns:ns1="http://tempuri.org/"
                xmlns:diffgr="urn:schemas-microsoft-com:xml-diffgram-v1"
                exclude-result-prefixes="xs fn fo">
    <xsl:output method="xml" encoding="UTF-8" indent="yes"/>

    <xsl:template match="/">
        <xsl:copy>
            <xsl:copy-of select="@*"/>
            <xsl:apply-templates/>
        </xsl:copy>
    </xsl:template>

    <xsl:template match="/soap:Envelope/soap:Body/ns1:NotifyContactState/ns1:notificationList/xs:schema"/>
    <xsl:template match="/soap:Envelope/soap:Body/ns1:NotifyContactState/ns1:notificationList/diffgr:diffgram">
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="*">
        <xsl:copy>
            <xsl:copy-of select="@*"/>
            <xsl:apply-templates/>
        </xsl:copy>
    </xsl:template>
</xsl:stylesheet>