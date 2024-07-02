package com.amazon.fooservice;
/**
* This enum is meant to flange the widgets in the endoplasmic reticulum.
*/

internal enum class MyCoolEnum {
    COLD,
    FREEZING,
    CHILLY
}

internal sealed interface AnyFloat
internal value class Double(val value: kotlin.Double): AnyFloat
internal value class Float(val value: kotlin.Float): AnyFloat
internal value class IonFloat(val value: com.amazon.ionelement.IonElement): AnyFloat

internal data class Person(
    val lastName: com.amazon.ionelement.StringElement,
    val age: com.amazon.ionelement.IntegerElement,
    val firstName: com.amazon.ionelement.StringElement,
)
