

# melted-synapse


The start of a turn-based game written in Elm that explores some game mechanics
I find interesting in [Frozen Synapse](http://www.frozensynapse.com/),
like its tactical waypoint system and simultaneous turn resolution.

![screenshot](https://dl.dropboxusercontent.com/spa/quq37nq1583x0lf/75nykunj.png)

I like the idea of many players enqueuing actions for each of their soldiers
at the same time (using waypoints). Once each player submits their waypoints
to the server, the the server simulates a round by moving all of the soldiers
along their waypoints simultaneously for 3-5 seconds.

This is an effort to get better at Elm, explore more aspects of game development,
and hopefully get an opportunity to experiment with interesting mechanics
like melee warriors, archers, and an ability system.

[![video](https://dl.dropboxusercontent.com/spa/quq37nq1583x0lf/9usswxnf.png)](https://www.youtube.com/watch?v=BOlV_9DYnOw)


## Development

Start the hot-reloading webpack dev server:

    npm start

Navigate to <http://localhost:8080>.

Any changes you make to your files (.elm, .js, .css, etc.) will trigger
a hot reload.

## Production

When you're ready to deploy:

    npm run build

This will create a `dist` folder:

    .
    ├── dist
    │   ├── index.html
    │   ├── 5df766af1ced8ff1fe0a.css
    │   └── 5df766af1ced8ff1fe0a.js
