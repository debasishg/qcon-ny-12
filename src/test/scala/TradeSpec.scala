package net.debasishg.domain.trade
package model

import org.scalatest.PropSpec
import org.scalatest.prop.{PropertyChecks, Checkers}
import org.scalatest.matchers.ShouldMatchers

import org.scalacheck.Arbitrary._
import org.scalacheck._
import Prop.forAll
import Gen._
import Arbitrary.arbitrary

import TradeModel._

object TradeGen {
  implicit lazy val arbMarket: Arbitrary[Market] = 
    Arbitrary(oneOf(value(HongKong), value(Singapore), value(Tokyo), value(NewYork), value(Other)))

  implicit lazy val arbTrade: Arbitrary[Trade] =
    Arbitrary {
      for {
        a <- Gen.oneOf("acc-01", "acc-02", "acc-03", "acc-04") 
        i <- Gen.oneOf("ins-01", "ins-02", "ins-03", "ins-04") 
        r <- Gen.oneOf("r-001", "r-002", "r-003")
        m <- arbitrary[Market]
        u <- Gen.oneOf(BigDecimal(1.5), BigDecimal(2), BigDecimal(10)) 
        q <- Gen.oneOf(BigDecimal(100), BigDecimal(200), BigDecimal(300)) 
      } yield Trade(a, i, r, m, u, q)
    }
}

import TradeGen._
class TradeSpecification extends PropSpec with PropertyChecks with ShouldMatchers {
  property("enrichment should result in netvalue > 0") {
    forAll((a: Trade) => 
      enrichTrade(a).netAmount.get should be > (BigDecimal(0)))
  }

  property("enrichment should mean netValue equals principal + taxes") {
    forAll((a: Trade) => { 
      val et = enrichTrade(a)
      et.netAmount should equal (et.taxFees.map(_.foldLeft(principal(et))((a, b) => a + b._2)))
    })
  }
} 

