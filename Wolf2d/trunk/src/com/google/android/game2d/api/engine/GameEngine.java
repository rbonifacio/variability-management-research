package com.google.android.game2d.api.engine;

import android.app.Activity;

import com.google.android.game2d.api.scene.Scene;

/**
 * Main class of a game, responsible for implementing the 
 * main loop and initialization methods. 
 * 
 * @author rbonifacio
 */
public abstract class GameEngine {
	protected Activity activity;
	protected boolean paused;
	protected Scene currentScene;
	protected SceneEngine sceneEngine;
	
	
	public GameEngine(Activity activity) {
		this.activity = activity;
	}
	
	/**
	 * Initialize the game and its resources, such as 
	 * TimeEngine and SoundEngine.
	 */
	public  void initialize() {
		paused = false;
		TimeEngine.instance().initialize();
		currentScene = sceneEngine.getSceneAt(0);
	}
	
	/**
	 * Update the current scene by the scene index
	 * 
	 * @param idx - scene index
	 */
	public void setCurrentScene(int idx) {
		currentScene = sceneEngine.getSceneAt(idx);
		currentScene.initialize();
		TimeEngine.instance().reset();
	}
	
	/**
	 * The main game loop. Responsible for updating 
	 * all state of the game. It means that devices 
	 * (TimeEngine, SoundEngine, ...) and current scene 
	 * must be updated.
	 */
	public void gameLoop() {
		TimeEngine.instance().update();
		
		currentScene.draw();
		if(!paused) {
			currentScene.update();
			currentScene.execute();
		}
	}
	
	/**
	 * This method is responsible for 
	 * pausing the game.
	 */
	public void pause() {
		paused = true;
	}
	
	public void resume() {
		TimeEngine.reset();
		paused = false;
	}
	
}
