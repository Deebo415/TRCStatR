// www/js/PlayerCard.js
class PlayerCard extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      tooltipContent: null,
      tooltipPosition: { x: 0, y: 0 }
    };
  }
  
  // Stat explanation data for tooltips
  statExplanations = {
    "AVG": "Batting Average shows how often you get a hit. If you bat 10 times and get 3 hits, your average is .300 - which is really good!",
    "HR": "Home Runs are when you hit the ball over the outfield fence. These are worth watching on replay!",
    "RBI": "Runs Batted In means how many times your hit helped a teammate score. It's a team stat that shows you're helping the team win!",
    "OBP": "On-Base Percentage includes all the ways you can get on base - hits, walks, and being hit by a pitch. The higher, the better!",
    "SLG": "Slugging percentage shows how powerful your hits are. Singles count a little, but doubles, triples, and home runs count a lot more!",
    "OPS": "On-base Plus Slugging adds together how often you get on base and how powerful your hits are. Over 1.000 is superstar level!",
    "WAR": "Wins Above Replacement shows how many more wins your team gets by having you play instead of a basic player. Higher is better!",
    "wRC": "Weighted Runs Created shows how many runs you helped create for your team with all your batting skills combined.",
    "SB": "Stolen Bases happen when you're fast enough to run to the next base when the pitcher isn't looking!",
    "ERA": "Earned Run Average shows how many runs a pitcher gives up per game. Lower is better - under 3.00 is awesome!",
    "WHIP": "Walks plus Hits per Inning Pitched shows how many baserunners a pitcher allows. Below 1.20 is really good!",
    "K": "Strikeouts are when a pitcher gets the batter out by throwing three strikes. More strikeouts means a stronger pitcher!",
    "BB": "Walks or Bases on Balls happen when a pitcher throws four balls outside the strike zone. Good pitchers have fewer walks.",
    "W": "Wins are credited to the pitcher when their team wins and they pitched well enough to deserve it.",
    "L": "Losses are given to the pitcher when their team loses and they gave up the runs that caused the loss.",
    "SV": "Saves happen when a pitcher finishes a close game and preserves the lead. Closers get a lot of these!"
  };
  
  // Show tooltip on hover
  showTooltip = (stat, event) => {
    this.setState({
      tooltipContent: this.statExplanations[stat],
      tooltipPosition: { 
        x: event.clientX, 
        y: event.clientY 
      }
    });
  };
  
  // Hide tooltip
  hideTooltip = () => {
    this.setState({
      tooltipContent: null
    });
  };

  // Format batting average, on-base percentage, etc.
  formatAvg = (value) => {
    return value.toFixed(3).toString().substring(1);
  };

  // Create rank indicator
  getRankBadge = (rank) => {
    const badgeClasses = rank <= 3 ? 
      (rank === 1 ? "rank-gold" : rank === 2 ? "rank-silver" : "rank-bronze") : 
      "rank-normal";
      
    return React.createElement("span", { className: `rank-badge ${badgeClasses}` }, "#" + rank);
  };

  // Create a stat item with tooltip
  renderStatItem = (label, value, rank, format = "number") => {
    let displayValue;
    
    switch (format) {
      case "avg":
        displayValue = this.formatAvg(value);
        break;
      case "ip":
        displayValue = value.toFixed(1);
        break;
      default:
        displayValue = value;
    }
    
    return React.createElement("div", {
      className: "stat-item",
      onMouseEnter: (e) => this.showTooltip(label, e),
      onMouseLeave: this.hideTooltip
    }, [
      React.createElement("span", { className: "stat-label" }, label),
      React.createElement("div", { className: "stat-value-container" }, [
        React.createElement("span", { className: "stat-value" }, displayValue),
        rank && this.getRankBadge(rank)
      ])
    ]);
  };

  render() {
    const { playerData } = this.props;
    const { tooltipContent, tooltipPosition } = this.state;
    
    if (!playerData) {
      return React.createElement("div", { className: "player-card-error" }, 
        "No player data available"
      );
    }
    
    return React.createElement("div", { 
      className: "player-card",
      style: {
        background: `linear-gradient(135deg, ${playerData.teamColors.primary} 0%, ${playerData.teamColors.secondary} 100%)`
      }
    }, [
      // Header
      React.createElement("div", { className: "card-header" }, [
        React.createElement("div", { className: "team-logo" }, 
          playerData.teamName
        ),
        React.createElement("div", { className: "player-identity" }, [
          React.createElement("h1", { className: "player-name" }, 
            `${playerData.firstName} ${playerData.lastInitial}.`
          ),
          React.createElement("div", { className: "jersey-number" }, 
            `#${playerData.jerseyNumber}`
          )
        ])
      ]),
      
      // Batting Stats
      React.createElement("div", { className: "stats-section" }, [
        React.createElement("h2", { className: "section-title" }, "Batting"),
        
        // Primary batting stats
        React.createElement("div", { className: "primary-stats" }, [
          React.createElement("div", { className: "stat-box" }, [
            React.createElement("div", { className: "stat-label" }, "AVG"),
            React.createElement("div", { className: "stat-value-lg" }, 
              this.formatAvg(playerData.batting.avg)
            ),
            this.getRankBadge(playerData.batting.ranks.avg)
          ]),
          React.createElement("div", { className: "stat-box" }, [
            React.createElement("div", { className: "stat-label" }, "HR"),
            React.createElement("div", { className: "stat-value-lg" }, 
              playerData.batting.hr
            ),
            this.getRankBadge(playerData.batting.ranks.hr)
          ]),
          React.createElement("div", { className: "stat-box" }, [
            React.createElement("div", { className: "stat-label" }, "RBI"),
            React.createElement("div", { className: "stat-value-lg" }, 
              playerData.batting.rbi
            ),
            this.getRankBadge(playerData.batting.ranks.rbi)
          ])
        ]),
        
        // Secondary batting stats
        React.createElement("div", { className: "secondary-stats" }, [
          this.renderStatItem("OBP", playerData.batting.obp, playerData.batting.ranks.obp, "avg"),
          this.renderStatItem("SLG", playerData.batting.slg, playerData.batting.ranks.slg, "avg"),
          this.renderStatItem("OPS", playerData.batting.ops, playerData.batting.ranks.ops, "avg"),
          this.renderStatItem("SB", playerData.batting.sb),
          this.renderStatItem("WAR", playerData.batting.war),
          this.renderStatItem("wRC", playerData.batting.wrc)
        ])
      ]),
      
      // Pitching Stats (if applicable)
      playerData.hasPitchingStats && React.createElement("div", { className: "stats-section pitching" }, [
        React.createElement("h2", { className: "section-title" }, "Pitching"),
        
        // Primary pitching stats
        React.createElement("div", { className: "primary-stats" }, [
          React.createElement("div", { className: "stat-box" }, [
            React.createElement("div", { className: "stat-label" }, "ERA"),
            React.createElement("div", { className: "stat-value-lg" }, 
              playerData.pitching.era.toFixed(2)
            ),
            this.getRankBadge(playerData.pitching.ranks.era)
          ]),
          React.createElement("div", { className: "stat-box" }, [
            React.createElement("div", { className: "stat-label" }, "K"),
            React.createElement("div", { className: "stat-value-lg" }, 
              playerData.pitching.k
            ),
            this.getRankBadge(playerData.pitching.ranks.k)
          ]),
          React.createElement("div", { className: "stat-box" }, [
            React.createElement("div", { className: "stat-label" }, "W-L"),
            React.createElement("div", { className: "stat-value-lg" }, 
              `${playerData.pitching.w}-${playerData.pitching.l}`
            )
          ])
        ]),
        
        // Secondary pitching stats
        React.createElement("div", { className: "secondary-stats" }, [
          this.renderStatItem("WHIP", playerData.pitching.whip, playerData.pitching.ranks.whip, "avg"),
          this.renderStatItem("IP", playerData.pitching.ip, null, "ip"),
          this.renderStatItem("BB", playerData.pitching.bb),
          this.renderStatItem("SV", playerData.pitching.sv)
        ])
      ]),
      
      // Footer
      React.createElement("div", { className: "card-footer" }, 
        "TRC Baseball 2025 Season"
      ),
      
      // Tooltip
      tooltipContent && React.createElement("div", {
        className: "stat-tooltip",
        style: {
          left: `${tooltipPosition.x + 10}px`,
          top: `${tooltipPosition.y - 10}px`,
          transform: 'translateY(-100%)'
        }
      }, tooltipContent)
    ]);
  }
}