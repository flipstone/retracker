#
# Application environment defaults
# These should be set by the (environment) task, run first
#
# set :snap_env, 'development'

require 'flipstone-deployment/capistrano/snap'

set :instance, "localhost"
set :branch, "master"
set :deployment_safeword, "tracketytrack"
set :executable, "retracker"

set :nginx_cfg, { port: 80 }

#
# environment settings (by task)
#
desc "Runs any following tasks to production environment"
task :production do
  #sanity_check
  set :snap_env, "production"
  set :instance, "retracker.flipstone.com"
  set :snap_port, 22099
  set_env
end

desc "Sets Capistrano environment variables after environment task runs"
task :set_env do
  role :web,      "#{instance}"
  role :app,      "#{instance}"
  role :db,       "#{instance}", :primary => true

  set :application, "retracker-#{snap_env}"
  set :deploy_to, "/srv/#{application}"
  set :scm_command, "GIT_SSH=#{deploy_to}/git_ssh.sh git"
  set :scm_passphrase, ""
  set :deploy_via, :remote_cache
  set :repository, "git@github.com:flipstone/retracker.git"
  set :use_sudo, false
  set :user, "ubuntu"
  set :scm, "git"
  set :local_scm_command, "git"

  ssh_options[:keys] = ["#{ENV['HOME']}/.ssh/fs-remote.pem"]
  ssh_options[:paranoid] = false
  ssh_options[:user] = "ubuntu"

  default_run_options[:pty] = true
end

before 'deploy:symlink', 'config:symlink'

namespace :config do
  task :symlink do
    run "ln -sf #{shared_path}/system/destinations #{release_path}/resources/config/destinations"
  end
end
