// TRC Baseball League Standings Widget
document.addEventListener('DOMContentLoaded', function() {
    const standingsContent = document.getElementById('standings-content');
    const logoPath = 'logos/'; // Path to team logos
    const dataPath = 'data.json'; // Path to standings data JSON
    
    // Function to fetch and render the standings data
    function fetchAndRenderStandings() {
        fetch(dataPath)
            .then(response => {
                if (!response.ok) {
                    throw new Error('Network response was not ok');
                }
                return response.json();
            })
            .then(data => {
                // Sort the data by winning percentage (descending), then run differential, then runs scored
                data.sort((a, b) => {
                    if (b.WinningPct !== a.WinningPct) return b.WinningPct - a.WinningPct;
                    if (b.RunDifferential !== a.RunDifferential) return b.RunDifferential - a.RunDifferential;
                    if (b.RunsScored !== a.RunsScored) return b.RunsScored - a.RunsScored;
                    return a.RunsAgainst - b.RunsAgainst;
                });
                
                // Create the table HTML
                let tableHtml = `
                    <table class="standings-table">
                        <thead>
                            <tr>
                                <th style="width: 60px;"></th>
                                <th style="text-align: left;">Team</th>
                                <th>W</th>
                                <th>L</th>
                                <th>T</th>
                                <th>PCT</th>
                                <th>RS</th>
                                <th>RA</th>
                                <th>DIFF</th>
                            </tr>
                        </thead>
                        <tbody>
                `;
                
                // Add each team to the table
                data.forEach(team => {
                    const runDiffClass = team.RunDifferential > 0 ? "run-diff-positive" : 
                                        (team.RunDifferential < 0 ? "run-diff-negative" : "run-diff-zero");
                    const runDiffDisplay = team.RunDifferential > 0 ? `+${team.RunDifferential}` : team.RunDifferential;
                    const logoFilename = team.LogoFilename || team.TeamName.toLowerCase().replace(/ /g, "_");
                    
                    tableHtml += `
                        <tr>
                            <td style="text-align: center;">
                                <img src="${logoPath}${logoFilename}.png" class="team-logo" alt="${team.TeamName} logo">
                            </td>
                            <td class="team-name">${team.TeamName}</td>
                            <td>${team.Wins}</td>
                            <td>${team.Losses}</td>
                            <td>${team.Ties}</td>
                            <td>${team.PCT}</td>
                            <td>${team.RunsScored}</td>
                            <td>${team.RunsAgainst}</td>
                            <td class="${runDiffClass}">${runDiffDisplay}</td>
                        </tr>
                    `;
                });
                
                tableHtml += `
                        </tbody>
                    </table>
                    <div style="text-align: right; font-size: 0.8em; margin-top: 5px; opacity: 0.8;">
                        Last updated: ${new Date().toLocaleString()}
                    </div>
                `;
                
                // Update the content
                standingsContent.innerHTML = tableHtml;
            })
            .catch(error => {
                console.error('Error fetching standings data:', error);
                standingsContent.innerHTML = `
                    <div class="error-message">
                        Unable to load standings data. Please try again later.
                    </div>
                `;
            });
    }
    
    // Initial load
    fetchAndRenderStandings();
    
    // Auto-refresh every hour (3600000 ms)
    setInterval(fetchAndRenderStandings, 3600000);
});
