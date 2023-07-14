# Fair Exchange Examples
Here you can find examples for the fair exchange check.
This check ensures, that if an action in the start tag is executed, the action in the stop tag is executed as well.
In the incorrect example, the &lt;&lt;fairexchange&gt;&gt; stereotype has the action `Pick Up` in the stop tag which has to be executed after the action `Pay` from the start tag was executed.
Since there is a decision node before the `Pick Up` action, it is not executed in every path within the diagram and a check will fail.
One can correct the model either by adding an action `Pick Up` to the other path after the decision node or add the action `Reclaim` to the stop tag.
In the correct model the action `Reclaim` was added into the stop tag.
So now, in every path one of the stop actions is executed and the check will be successful.
