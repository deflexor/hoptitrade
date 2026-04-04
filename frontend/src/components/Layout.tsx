import { Outlet, NavLink, useNavigate } from 'react-router-dom';
import { ModeToggle } from '@/components/ModeToggle';
import { useAuthStore } from '@/stores';
import { Button } from '@/components/ui/button';
import { Separator } from '@/components/ui/separator';

export function Layout() {
  const { logout } = useAuthStore();
  const navigate = useNavigate();

  const handleLogout = () => {
    logout();
    navigate('/login');
  };

  return (
    <div className="min-h-screen bg-background">
      {/* Top Bar */}
      <header className="sticky top-0 z-50 w-full border-b bg-background/95 backdrop-blur supports-[backdrop-filter]:bg-background/60">
        <div className="container flex h-14 items-center">
          <div className="mr-4 flex">
            <NavLink to="/" className="mr-6 flex items-center space-x-2">
              <span className="text-xl font-bold">HoptiTrade</span>
            </NavLink>
            <nav className="flex items-center space-x-6 text-sm font-medium">
              <NavLink
                to="/opportunities"
                className={({ isActive }) =>
                  isActive ? 'text-foreground' : 'text-foreground/60 transition-colors hover:text-foreground'
                }
              >
                Opportunities
              </NavLink>
              <NavLink
                to="/positions"
                className={({ isActive }) =>
                  isActive ? 'text-foreground' : 'text-foreground/60 transition-colors hover:text-foreground'
                }
              >
                Open Positions
              </NavLink>
              <NavLink
                to="/settings"
                className={({ isActive }) =>
                  isActive ? 'text-foreground' : 'text-foreground/60 transition-colors hover:text-foreground'
                }
              >
                Settings
              </NavLink>
            </nav>
          </div>
          <div className="flex flex-1 items-center justify-between space-x-2 md:justify-end">
            <div className="flex items-center space-x-4">
              <ModeToggle />
              <Separator orientation="vertical" className="h-6" />
              <Button variant="ghost" size="sm" onClick={handleLogout}>
                Logout
              </Button>
            </div>
          </div>
        </div>
      </header>

      {/* Main Content */}
      <main className="container py-6">
        <Outlet />
      </main>
    </div>
  );
}
