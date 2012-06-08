package net.debasishg.domain.trade
package service

import event.{InMemoryEventLog, RedisEventLog}
import org.scalatest.{Spec, BeforeAndAfterAll}
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import akka.pattern.ask
import akka.util.Timeout
import akka.util.duration._

import com.redis._

@RunWith(classOf[JUnitRunner])
class TradeLifecycleSpec extends Spec with ShouldMatchers with BeforeAndAfterAll {
  import java.util.Calendar
  import akka.actor.{Actor, ActorRef, Props, ActorSystem, FSM}
  import akka.dispatch.Await
  import akka.util.Timeout
  import akka.util.duration._
  import akka.routing.Listen
  import Actor._
  import FSM._
  import model.TradeModel._

  val system = ActorSystem("TradingSystem")
  // implicit val timeout = system.settings.ActorTimeout
  implicit val timeout = Timeout(20 seconds)
  override def afterAll = { system.shutdown() }

  describe("trade lifecycle") {
    it("should work with in memory event logging") {
      val log = new InMemoryEventLog(system)
      val finalTrades = new collection.mutable.ListBuffer[Trade]

      // make trades
      val trds = 
        List(
          Trade("a-123", "google", "r-123", HongKong, 12.25, 200),
          Trade("a-124", "ibm", "r-124", Tokyo, 22.25, 250),
          Trade("a-125", "cisco", "r-125", NewYork, 20.25, 150),
          Trade("a-126", "ibm", "r-127", Singapore, 22.25, 250))

      // set up listeners
      val qry = system.actorOf(Props(new TradeQueryStore))

      // do service
      trds.foreach {trd =>
        val tlc = system.actorOf(Props(new TradeLifecycle(trd, timeout.duration, Some(log))))
        tlc ! SubscribeTransitionCallBack(qry)
        tlc ! AddValueDate
        tlc ! EnrichTrade
        val future = tlc ? SendOutContractNote
        finalTrades += Await.result(future, timeout.duration).asInstanceOf[Trade]
      }
      Thread.sleep(1000)

      // get snapshot
      import TradeSnapshot._
      val trades = snapshot(log, system)
      finalTrades should equal(trades)

      // check query store
      val f = qry ? QueryAllTrades
      val qtrades = Await.result(f, timeout.duration).asInstanceOf[List[Trade]]
      qtrades should equal(finalTrades)
    }

    it("should work with redis based event logging") {
      val clients = new RedisClientPool("localhost", 6379)
      val log = new RedisEventLog(clients, system)
      val finalTrades = new collection.mutable.ListBuffer[Trade]

      // make trades
      val trds = 
        List(
          Trade("a-123", "google", "r-123", HongKong, 12.25, 200),
          Trade("a-124", "ibm", "r-124", Tokyo, 22.25, 250),
          Trade("a-125", "cisco", "r-125", NewYork, 20.25, 150),
          Trade("a-126", "ibm", "r-127", Singapore, 22.25, 250))

      // set up listeners
      val qry = system.actorOf(Props(new TradeQueryStore))

      // do service
      trds.foreach {trd =>
        val tlc = system.actorOf(Props(new TradeLifecycle(trd, timeout.duration, Some(log))))
        tlc ! SubscribeTransitionCallBack(qry)
        tlc ! AddValueDate
        tlc ! EnrichTrade
        val future = tlc ? SendOutContractNote
        finalTrades += Await.result(future, timeout.duration).asInstanceOf[Trade]
      }
      Thread.sleep(1000)

      // get snapshot
      import TradeSnapshot._
      val trades = snapshot(log, system)
      finalTrades should equal(trades)

      // check query store
      val f = qry ? QueryAllTrades
      val qtrades = Await.result(f, timeout.duration).asInstanceOf[List[Trade]]
      qtrades should equal(finalTrades)
      clients.withClient{ client => client.flushdb }
      clients.withClient {client => client.disconnect}
      clients.close
    }
  }
}
