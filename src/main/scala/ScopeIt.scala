package bobsrockets {
  package navigation {
    private[bobsrockets] class Navigator {
      protected[navigation] def useStarChart() = {}
      class LegOfJourney {
        private[Navigator] val distance = 100
      }
      /**
        * distance can be used here because of the private[Navigator]
        */
      private[this] val speed = 200 * (new LegOfJourney()).distance
      def wow(that: Navigator): Int = {
        this.speed
        /**
          * that.speed is not allowed, because of the private[this]
          */
        // that.speed
      }
    }

    class Navigator2 {
      /**
        * useStarChart can be used here because of the protected[navigation]
        */
      def hi() = (new Navigator()).useStarChart()
    }
  }

  package launch {
    import navigation._
    object Vehicle {
      private[launch] val guide = new Navigator
    }
    class Vehicle2 {
      /**
        * guide can be used here because of the private[launch]
        */
      val g = Vehicle.guide;
    }
  }
}

class ScopeIt {
}
