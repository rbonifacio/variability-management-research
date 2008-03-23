package com.google.android.game2d.api.engine;

import com.google.android.game2d.api.scene.Scene;

/**
 * Each game must provide a scene engine implementation,
 * responsible for loading indexed scenes. Possible implementations 
 * might retrieve scenes by reading from a configuration file. This 
 * interface was proposed just to decouple GameEngine from the list 
 * of available scenes.
 * 
 * @author rbonifacio - rba2@cin.ufpe.br
 */
public interface  SceneEngine {

	public Scene getSceneAt(int idx);

}
