library(rsconnect)

rsconnect::setAccountInfo(name = 'ilsnap-ed',
                          token = 'ENTER_TOKEN',
                          secret = 'ENTER_SECRET')
deployApp("C:\\Users\\user\\path\\to\\communitynetworksdashboard\\CommunityNetworksDashboard",
          launch.browser = F,
          forceUpdate = T,
          appName = 'community_networks_dashboard',
          account = "ilsnap-ed")